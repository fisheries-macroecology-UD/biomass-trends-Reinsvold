  # 0.2 DFA - Fit One Trend DFA Model to Subregions
  
  library(bayesdfa)
  library(dplyr)
  library(purrr)
  library(ggplot2)
  
  # define regions to run independently (US subregions + Canada)
  regions <- c(
    "Bering Sea",
    "Gulf of Alaska",
    "California Current",
    "Canada"
  )
  
  # return data for one region, 1993 onward
  get_region_data <- function(region) {
    dat <- if (region == "Canada") {
      Canada
    } else {
      filter(biomass_dat, subregion == region)
    }
    filter(dat, year >= 1993)
  }
  
  # loop through each region separately
  walk(regions, \(region) {
    
    # filter out stocks with fewer than 10 years of data
    dat_dfa <- get_region_data(region) |>
      select(common_name, year, value) |>
      group_by(common_name) |>
      filter(n() >= 10) |>
      ungroup()
    
    # format data for bayesdfa: create time index and rename columns
    dat_dfa <- dat_dfa |>
      arrange(common_name, year) |>
      mutate(time = year - min(year) + 1) |>
      arrange(common_name, time) |>
      rename(obs = value, ts = common_name) |>
      select(-year) |>
      ungroup()
    
    # create numeric IDs for species required by bayesdfa
    ts_key <- distinct(dat_dfa, ts) |>
      mutate(ts_id = row_number())
    
    # replace species names with numeric IDs and keep DFA input columns
    dat_dfa <- dat_dfa |>
      left_join(ts_key, by = "ts") |>
      mutate(ts = ts_id) |>
      select(time, ts, obs)
    
    # set seed
    set.seed(650)
    
    # fit model with one trend
    fit_long <- fit_dfa(
      y = dat_dfa,
      num_trends = 1,
      scale = "zscore",
      iter = 10000,
      chains = 1,
      thin = 1,
      data_shape = "long",
      estimation = "sampling",
      control = list(adapt_delta = 0.99)
    )
    
    # save DFA model and species key
    saveRDS(fit_long, file = paste0("./output/", region, ".rds"))
    saveRDS(ts_key,   file = paste0("./output/", region, "_ts_key.rds"))
  })
  
  # choose region to plot (any entry in regions)
  region <- "California Current"
  
  # load DFA model and species key
  tmp_plot <- readRDS(paste0("./output/", region, ".rds"))
  ts_key   <- readRDS(paste0("./output/", region, "_ts_key.rds"))
  
  # recreate data for plotting
  plot_dat <- get_region_data(region)
  
  # check convergence
  is_converged(tmp_plot, threshold = 1.2)
  
  # invert trend so majority of species fit positive
  r <- rotate_trends(tmp_plot, invert = TRUE)
  
  # define years and species names
  yrs <- seq(min(plot_dat$year), max(plot_dat$year))
  spp_names <- ts_key$ts[order(ts_key$ts_id)]
  
  # build the three DFA plots
  plots <- list(
    trends   = plot_trends(r, years = yrs) + theme_bw(),
    fitted   = plot_fitted(tmp_plot, names = spp_names) + theme_bw(),
    loadings = plot_loadings(r, names = spp_names) +
      theme_bw() +
      scale_alpha_continuous(
        name   = "P(loading ≠ 0)",
        limits = c(0.5, 1),
        breaks = c(0.5, 0.75, 1),
        range  = c(0.2, 1),
        guide  = guide_legend(override.aes = list(fill = "red"))
      ) +
      guides(fill = "none")
  )
  
  # show them
  print(plots$trends)
  print(plots$fitted)
  print(plots$loadings)
  
  # save them
  for (nm in names(plots)) {
    ggsave(
      filename = paste0("./output/", region, "_", nm, ".png"),
      plot     = plots[[nm]],
      width    = 8, height = 5, dpi = 300, limitsize = FALSE
    )
  }
