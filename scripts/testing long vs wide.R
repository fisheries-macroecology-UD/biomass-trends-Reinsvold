
library(bayesdfa)
library(ggplot2)
library(dplyr)
library(rstan)
chains = 1
iter = 1000

set.seed(1)
sim_dat <- sim_dfa(
  num_trends = 2,
  num_years = 20,
  num_ts = 4
)

fit_wide <- fit_dfa(
  y = sim_dat$y_sim, num_trends = 1, scale="zscore",
  iter = iter, chains = chains, thin = 1
)

is_converged(fit_wide, threshold = 1.05)

r_wide <- rotate_trends(fit_wide)
plot_trends(r_wide) + theme_bw()
plot_fitted(fit_wide) + theme_bw()
plot_loadings(r_wide) + theme_bw()

# turn into long format

d_long <- as.data.frame(sim_dat$y) |>
  mutate(ts = row_number()) |>
  pivot_longer(
    cols = starts_with("V"),
    names_to = "time",
    values_to = "obs"
  ) |>
	dplyr::mutate(time = as.integer(sub("V", "", time)))

fit_long <- fit_dfa(
  y = d_long, num_trends = 1, scale="zscore",
  iter = iter, chains = chains, thin = 1, data_shape = "long"
)

is_converged(fit_long, threshold = 1.05)

r_long <- rotate_trends(fit_long)
plot_trends(r_long) + theme_bw()
plot_fitted(fit_long) + theme_bw()
plot_loadings(r_long)

#### same but inverted - how do i fix? 