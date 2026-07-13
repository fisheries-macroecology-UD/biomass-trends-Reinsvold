library(bayesdfa)
library(dplyr)
library(tidyr)
library(tibble)

# ---- Build wide matrix from existing dat_dfa ----
y_mat <- dat_dfa |>
  pivot_wider(names_from = time, values_from = obs) |>
  column_to_rownames("ts") |>
  as.matrix()

# ---- Search over number of trends ----
set.seed(652)
m <- find_dfa_trends(
  y = y_mat,
  kmin = 1,
  kmax = 3,
  iter = 10000,
  thin = 1,
  chains = 1,
  compare_normal = FALSE,
  convergence_threshold = 1.2,
  variance = c("equal", "unequal"),
  control = list(adapt_delta = 0.99, max_treedepth = 25)
)

library(loo)

# ---- Load the fits ----
fit_west  <- readRDS("./output/west.rds")
fit_west2 <- readRDS("./output/west2.rds")
fit_west3 <- readRDS("./output/west3.rds")

# ---- LOOIC for each ----
loo_west  <- loo(fit_west)
loo_west2 <- loo(fit_west2)
loo_west3 <- loo(fit_west3)

loo_west
loo_west2
loo_west3

# ---- Compare ----
loo::loo_compare(loo_west, loo_west2, loo_west3)
