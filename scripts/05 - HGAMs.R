# explore HGAMs

  library(mgcv)
  
  #### fit HGAMs for each region ####
  
  # fit two models per region: one with a global trend (suggests shared trend across all stocks in a region) 
  # and one with group-level trends (suggests each stock has its own trend)
  
  # we also want to fit models where group-level smoothers have the same wiggliness (S in Pederson et al. 2019) 
  # and different wiggliness (I)
  
  
  ## California Current ---------------------------------------------------
  
  cc_dat <- biomass_dat |>
    filter(subregion == "California Current")
  
  # change group category to a factor
  cc_dat$stock_name <- as.factor(cc_dat$stock_name)
  
  # global-only model (G) ------------------------------------------------
  cc_mod_g <- gam(
    log(value) ~
      s(year, k = 5, m = 2) +
      s(stock_name, bs = "re"),
    data = cc_dat,
    method = "REML",
    family = scat(link = "identity")
  )
  
  # same wiggliness (GS vs S) --------------------------------------------
  
  # GS: global trend + stock-specific deviations
  # all stock deviations share smoothing parameters
  cc_mod_gs <- gam(
    log(value) ~
      s(year, bs = "tp", k = 5, m = 2) +
      s(year, stock_name, bs = "fs", k = 10, m = 2),
    data = cc_dat,
    method = "REML",
    family = scat(link = "identity")
  )
 
  # S: stock-specific trends with shared smoothing parameters,
  # but no global trend
  cc_mod_s <- gam(
    log(value) ~
      s(year, stock_name, bs = "fs", k = 10, m = 2),
    data = cc_dat,
    method = "REML",
    family = scat(link = "identity")
  )
  
  # compare
  AIC(cc_mod_g, cc_mod_gs, cc_mod_s)
  
  # checks
  
  # convergence and whether k is adequate
  gam.check(cc_mod_g)
  gam.check(cc_mod_gs)
  gam.check(cc_mod_s)
  
  # residual plots
  gratia::appraise(
    cc_mod_g,
    method = "simulate",
    n_simulate = 100,
    level = 0.95,
    seed = 123
  )
    
  gratia::appraise(
    cc_mod_gs,
    method = "simulate",
    n_simulate = 100,
    level = 0.95,
    seed = 123
  )
  
  gratia::appraise(
    cc_mod_s,
    method = "simulate",
    n_simulate = 100,
    level = 0.95,
    seed = 123
  )
  
  # concurvity: checks whether one model term can be approximated by the other terms
  concurvity(cc_mod_g, full = TRUE)
  concurvity(cc_mod_gs, full = TRUE)
  concurvity(cc_mod_s, full = TRUE)
  
  ## different wiggliness (GI vs i) --------------------------------------
  #
  ## GI: global trend plus stock deviations with individual wiggliness
  #cc_mod_gi <- gam(
  #  log(value) ~
  #    s(year, bs = "tp", k = 5, m = 2) +
  #    s(year, by = stock_name, bs = "tp", k = 10, m = 1) +
  #    s(stock_name, bs = "re"),
  #  data = cc_dat,
  #  method = "REML",
  #  family = scat(link = "identity")
  #)
  #
  ## I: independent stock trends with individual wiggliness
  #cc_mod_i <- gam(
  #  log(value) ~
  #    s(year, by = stock_name, bs = "tp", k = 10, m = 2) +
  #    s(stock_name, bs = "re"),
  #  data = cc_dat,
  #  method = "REML",
  #  family = scat(link = "identity")
  #)
  #
  ## compare
  #AIC(cc_mod_g, cc_mod_gi, cc_mod_i)
  #
  ## checks
  #
  ## convergence and whether k is adequate
  #gam.check(cc_mod_g)
  #gam.check(cc_mod_gi)
  #gam.check(cc_mod_i)
  #
  ## residual plots
  #gratia::appraise(
  #  cc_mod_gi,
  #  method = "simulate",
  #  n_simulate = 100,
  #  level = 0.95,
  #  seed = 123
  #)
  #
  #gratia::appraise(
  #  cc_mod_i,
  #  method = "simulate",
  #  n_simulate = 100,
  #  level = 0.95,
  #  seed = 123
  #)
  #
  #concurvity(cc_mod_gi, full = TRUE)
  #concurvity(cc_mod_i, full = TRUE)
  #
  ## compare all
  #mods <- list(
  #  G  = cc_mod_g,
  #  GS = cc_mod_gs,
  #  GI = cc_mod_gi,
  #  S  = cc_mod_s,
  #  I  = cc_mod_i
  #)
  #
  #aic_tab <- mods |>
  #  imap_dfr(\(model, model_name) {
  #    tibble(
  #      model = model_name,
  #      df = attr(logLik(model), "df"),
  #      AIC = AIC(model)
  #    )
  #  }) |>
  #  mutate(
  #    delta_AIC = AIC - min(AIC)
  #  ) |>
  #  arrange(AIC)
  #
  #aic_tab
#
  #
  
  ## Bering Sea ----------------------------------------------------------

  bs_dat <- biomass_dat |>
    filter(subregion == "Bering Sea") |>
    drop_na(value)
  
  bs_dat |>
    summarise(
      n = n(),
      missing_value = sum(is.na(value)),
      nonfinite_value = sum(!is.finite(value)),
      nonpositive_value = sum(value <= 0, na.rm = TRUE),
      missing_year = sum(is.na(year)),
      nonfinite_year = sum(!is.finite(year)),
      missing_stock = sum(is.na(stock_name))
    )
  
  bs_dat <- bs_dat |>
    dplyr::filter(value > 0) |>
    dplyr::mutate(
      stock_name = droplevels(factor(stock_name))
    )
  
  # change group category to a factor
  bs_dat$stock_name <- as.factor(bs_dat$stock_name)
  
  # global-only model (G) ------------------------------------------------
  bs_mod_g <- gam(
    log(value) ~
      s(year, k = 5, m = 2) +
      s(stock_name, bs = "re"),
    data = bs_dat,
    method = "REML",
    family = scat(link = "identity")
  )
  
  # same wiggliness (GS vs S) --------------------------------------------
  
  # GS: global trend + stock-specific deviations
  # all stock deviations share smoothing parameters
  bs_mod_gs <- gam(
    log(value) ~
      s(year, bs = "tp", k = 5, m = 2) +
      s(year, stock_name, bs = "fs", k = 10, m = 2),
    data = bs_dat,
    method = "REML",
    family = scat(link = "identity")
  )
  
  # S: stock-specific trends with shared smoothing parameters,
  # but no global trend
  bs_mod_s <- gam(
    log(value) ~
      s(year, stock_name, bs = "fs", k = 10, m = 2),
    data = bs_dat,
    method = "REML",
    family = scat(link = "identity")
  )
  
  # compare
  AIC(bs_mod_g, bs_mod_gs, bs_mod_s)
  
  
  ## GOA ----------------------------------------------------------
  
  goa_dat <- biomass_dat |>
    filter(subregion == "Gulf of Alaska")
  
  # change group category to a factor
  goa_dat$stock_name <- as.factor(goa_dat$stock_name)
  
  # global-only model (G) ------------------------------------------------
  goa_mod_g <- gam(
    log(value) ~
      s(year, k = 5, m = 2) +
      s(stock_name, bs = "re"),
    data = goa_dat,
    method = "REML",
    family = scat(link = "identity")
  )
  
  # same wiggliness (GS vs S) --------------------------------------------
  
  # GS: global trend + stock-specific deviations
  # all stock deviations share smoothing parameters
  goa_mod_gs <- gam(
    log(value) ~
      s(year, bs = "tp", k = 5, m = 2) +
      s(year, stock_name, bs = "fs", k = 10, m = 2),
    data = goa_dat,
    method = "REML",
    family = scat(link = "identity")
  )
  
  # S: stock-specific trends with shared smoothing parameters,
  # but no global trend
  goa_mod_s <- gam(
    log(value) ~
      s(year, stock_name, bs = "fs", k = 10, m = 2),
    data = goa_dat,
    method = "REML",
    family = scat(link = "identity")
  )
  
  # compare
  AIC(goa_mod_g, goa_mod_gs, goa_mod_s)
  