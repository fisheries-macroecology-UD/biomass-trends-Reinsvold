# 0.3 DFA - Fit DFA Model to All West Coast Regions (choose number of trends)
  
  # Sablefish was duplicated into Bering Sea earlier; keep only one copy for the model.
  biomass_dat <- biomass_dat |>
  filter(!(common_name == "Sablefish" & subregion == "Bering Sea"))

  # use stock_name as the unique stock identifier
  biomass_dat <- biomass_dat |>
    mutate(stock = stock_name)
  
  # choose number of trends (default 3) ####
  num_trends <- 3
  
  # filter out stocks with fewer than 10 years of data
  dat_dfa <- biomass_dat |>
    select(stock, year, value) |>
    group_by(stock) |>
    filter(n() >= 10) |>
    ungroup()
  
  # print how many distinct stocks are in the dataset before fitting
  cat("Number of distinct stocks entering the model:",
      n_distinct(dat_dfa$stock), "\n")
  
  # format data for bayesdfa: create time index and rename columns
  dat_dfa <- dat_dfa |>
    arrange(stock, year) |>
    mutate(time = year - min(year) + 1) |>
    arrange(stock, time) |>
    rename(obs = value, ts = stock) |>
    select(-year) |>
    ungroup()
  
  # create numeric IDs for stocks required by bayesdfa
  ts_key <- distinct(dat_dfa, ts) |>
    mutate(ts_id = row_number())
  
  # replace stock names with numeric IDs and keep DFA input columns
  dat_dfa <- dat_dfa |>
    left_join(ts_key, by = "ts") |>
    mutate(ts = ts_id) |>
    select(time, ts, obs)
  
  # set seed
  set.seed(650)
  
  # fit model with chosen number of trends
  fit_long <- fit_dfa(
    y = dat_dfa,
    num_trends = num_trends,
    scale = "zscore",
    iter = 10000,
    chains = 1,
    thin = 1,
    data_shape = "long",
    estimation = "sampling",
    control = list(adapt_delta = 0.99)
  )
  
  # save DFA model and stock key, labeled by number of trends
  saveRDS(fit_long, file = paste0("./output/westcoast_", num_trends, "trend.rds"))
  saveRDS(ts_key,   file = paste0("./output/westcoast_", num_trends, "trend_ts_key.rds"))
  
  
  #### plotting ####
  
  # choose which fitted model to plot
  num_trends <- 3
  
  # load model and stock key
  tmp_plot <- readRDS(paste0("./output/westcoast_", num_trends, "trend.rds"))
  ts_key   <- readRDS(paste0("./output/westcoast_", num_trends, "trend_ts_key.rds"))
  
  # check convergence
  is_converged(tmp_plot, threshold = 1.2)
  
  # invert trend so majority of stocks fit positive
  r <- rotate_trends(tmp_plot, invert = TRUE)
  
  flip <- 3
  r$Z_rot[, , flip]     <- -r$Z_rot[, , flip]
  r$trends[, flip, ]    <- -r$trends[, flip, ]
  r$trends_mean[flip, ] <- -r$trends_mean[flip, ]
  new_lower <- -r$trends_upper[flip, ]
  new_upper <- -r$trends_lower[flip, ]
  r$trends_lower[flip, ] <- new_lower
  r$trends_upper[flip, ] <- new_upper
  
  # years and stock names (in ts_id order)
  yrs <- seq(min(biomass_dat$year), max(biomass_dat$year))
  spp_names <- ts_key$ts[order(ts_key$ts_id)]
  
  # trends + fitted use built-in functions
  plots <- list(
    trends = plot_trends(r, years = yrs) + theme_bw(),
    fitted = plot_fitted(tmp_plot, names = spp_names) + theme_bw()
  )
  print(plots$trends)
  print(plots$fitted)
  
  #### combined loadings plot: per-stock violins, colored by subregion, alpha = P(loading ≠ 0), sorted by region ####
  
  # build a per-stock display label: "species - region" only when a common_name
  # spans multiple regions, otherwise just the species name
  label_key <- load_draws |>
    distinct(ts_id, common_name, subregion) |>
    add_count(common_name, name = "n_regions") |>
    mutate(stock_label = ifelse(n_regions > 1,
                                paste0(common_name, " - ", subregion),
                                as.character(common_name)))
  
  # ordering: by subregion (region), then species name
  order_key <- label_key |>
    arrange(subregion, common_name) |>
    distinct(stock_label, subregion)
  
  # attach labels and set factor order (rev so first region plots at top)
  load_draws_combined <- load_draws |>
    # drop any factor coding from the earlier per-species plot to avoid NA joins
    mutate(common_name = as.character(common_name)) |>
    left_join(label_key |> select(ts_id, stock_label), by = "ts_id") |>
    mutate(stock_label = factor(stock_label, levels = rev(order_key$stock_label)))
  
  # one violin per stock, colored by subregion, faceted by trend
  loadings_plot <- ggplot(load_draws_combined,
                          aes(x = loading, y = stock_label,
                              fill = subregion, alpha = prob_diff0)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_violin(scale = "width", width = 0.7, colour = NA, linewidth = 0.2) +
    scale_alpha_continuous(
      name   = "P(loading ≠ 0)",
      limits = c(0.5, 1),
      breaks = c(0.5, 0.75, 1),
      range  = c(0.2, 1),
      guide  = guide_legend(override.aes = list(fill = "grey20"))
    ) +
    scale_y_discrete(expand = expansion(add = 0.8)) +
    facet_wrap(~ trend) +
    labs(x = "Loading", y = "Species", fill = "Subregion") +
    theme_bw()
  
  print(loadings_plot)
  plots$loadings <- loadings_plot
  
  # save
  ggsave(
    filename = paste0("./output/westcoast_", num_trends, "trend_loadings.png"),
    plot     = loadings_plot,
    width    = 12,
    height   = max(8, 0.35 * n_distinct(load_draws_combined$stock_label)),
    dpi      = 300, limitsize = FALSE
  )