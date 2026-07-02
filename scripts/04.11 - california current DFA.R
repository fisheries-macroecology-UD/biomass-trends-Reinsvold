# DFA - looking for similar trends in biomass of groundfishes in EBS

# dat_dfa <- biomass_dat %>%
  # filter(year > max(year) - 50) |>      # keep only the most recent 50 years
  # select(common_name, year, value) |>
  # group_by(common_name) %>%
  # filter(n() >= 5) |>
  # ungroup()

reg <- grep("Cal", ecosystems, value = TRUE)
  # change for specific region

biomass_dat <- biomass_dat |>
  filter(regional_ecosystem %in% reg)

library(bayesdfa)
options(mc.cores = parallel::detectCores())

# dir.create("output", showWarnings = FALSE)   # would break: no output folder for saveRDS/ggsave

# get data set up

# filter out stocks with fewer than 20 years of data
dat_dfa <- biomass_dat %>%  
select(common_name, year, value) |>     # commonname -> common_name
    group_by(common_name) %>%                 # commonname -> common_name
 filter(n() >= 20) |>
ungroup()

 dat_dfa <- dat_dfa |>
        group_by(common_name, year) |>                                  # would break: collapse duplicate
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |># stock-year rows (cause of seq_len(P))
  arrange(common_name, year) |>           # commonname -> common_name
        mutate(time = year - min(year) + 1) |>
        arrange(common_name, time) |>           # commonname -> common_name
        rename(obs = value, ts = common_name) |># commonname -> common_name
  select(-year) |>
        ungroup()

  ts_key <- distinct(dat_dfa, ts) |> mutate(ts_id = row_number())  # would break: fit_dfa needs ts as integer
  dat_dfa <- dat_dfa |> left_join(ts_key, by = "ts") |>            # index, not names — swap ts to integer id
    mutate(ts = ts_id) |> select(time, ts, obs)                    # (the actual seq_len(P) fix)

  # set seed
  set.seed(650)
 
  # fit model with one trend
  fit_long <- fit_dfa(
    y = dat_dfa,
    num_trends = 1,
    scale="zscore",
    iter = 5000,
    chains = 1,
    thin = 1,
    data_shape = "long",
    estimation = "sampling",
    control = list(adapt_delta = 0.99)
    )
   
  # save
  saveRDS(fit_long, file = "./output/CCtrend.rds")
 
  fit_long <- readRDS( file = "./output/CCtrend.rds")
 
  # check convergence
  is_converged(fit_long, threshold = 1.05)   # fit -> fit_long

  # plot
  
  r <- rotate_trends(fit_long)
  yrs <- seq(min(biomass_dat$year), max(biomass_dat$year))
  spp_names <- ts_key$ts[order(ts_key$ts_id)]
  
  # flip sign convention for display
  r$trends_mean  <- -r$trends_mean
  r$trends_lower <- -r$trends_lower
  r$trends_upper <- -r$trends_upper

  # flip loadings to match
  r$Z_rot        <- -r$Z_rot
  r$Z_rot_mean   <- -r$Z_rot_mean
  
  #Three primary output plots
  plot_trends(r, years = yrs) + theme_bw() + ggtitle("California Current Biomass Trend")
 
  plot_fitted(fit_long, names = spp_names) + theme_bw() + ggtitle("California Current Biomass Fitted Trend")
 
  plot_loadings(r, names = spp_names) + theme_bw() + ggtitle("California Current Biomass Loadings")
 
#### plots ####
   
  ## trend plot ##
  yrs <- sort(unique(biomass_dat$year))   # biomass_envr_dat -> biomass_dat (undefined object)
 
trends <- data.frame(
 t(r$trends_mean),
 t(r$trends_lower),
 t(r$trends_upper),
 year = yrs)

n_trends <- ncol(t(r$trends_mean))

if (n_trends == 1) {
 colnames(trends) <- c("estimate", "lower", "upper", "year")
} else {
 colnames(trends) <- c(
   paste0("estimate", 1:n_trends),
   paste0("lower", 1:n_trends),
   paste0("upper", 1:n_trends),
   "year"
 )
}

trend <-
ggplot(trends, aes(x = year, y = estimate)) +
   geom_ribbon(aes(ymin = lower, ymax = upper),
                  alpha = 0.4, color = "#fffae0", fill= "#fffae0") +
   geom_line(color = "#fff3b2") + ## change to geom_path to connect across missing points
   #    facet_wrap("trend_number") +
   xlab("Year") +
   ylab("Trend") +
black_theme(x = 16, y = 16)

ggsave(trend, file = "./output/trend_plot.png",
height = 8, width = 12, units = "in")

# plot loadings

spp_names <- ts_key$ts[order(ts_key$ts_id)]   # was fit_long$orig_data$ts (now integers) — use names from ts_key

ld <- dfa_loadings(r,
summary = FALSE,
names = spp_names)

ld <- ld |>
  group_by(name) |>
  mutate(ord = median(loading, na.rm = TRUE)) |>
  ungroup() |>
  mutate(name = fct_reorder(name, ord))

loadings <-
ggplot(ld) +
 geom_hline(yintercept = 0, color = "lightgrey") +
      geom_violin(aes(x = name, y = loading,
fill = trend, color = "black", alpha = prob_diff0)) +
scale_fill_viridis_c() +
      coord_flip() +
      xlab("Species") +
      ylab("Loading") +
black_theme(x = 20, y = 16) +
theme(panel.grid.major = element_line("#323232"))

ggsave(loadings, file = "./output/loadings_plot.png",
height = 10, width = 10, units = "in")

