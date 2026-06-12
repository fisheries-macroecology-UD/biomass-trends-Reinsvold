	# fit dsem models for all EBS species of temp effect on bmioass with lags 0-30 years

	library(dplyr)
	library(ggplot2)
	library(forcats)
	library(glue)

	# species and lags
	species_list <- unique(biomass_envr_dat$commonname)
	lags <- c(0:25)
	
	# df for function
	grid <- tidyr::expand_grid(
  	species_list = species_list,
  	lags = lags)
	
	#### no effect of temperature ####
	
	# function to fit dsem models
	dsem_models_no_temp <- purrr::map(grid$species_list, \(sp){

		 # filter by species
		 df <- biomass_envr_dat |>
		 	filter(commonname == sp) |>
		 	select(commonname, year, value, mean_val) |>
		  rename(biomass = value,
		  			 mean_temp = mean_val) |>
		 	ungroup()
		 
		 df_ts <- df |>
  		dplyr::select(biomass, mean_temp)
		 
		 df_ts <- df_ts |>
		 	mutate(across(where(is.numeric), ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)))
		
		  # turn into timeseries df
		  df_ts <- ts(df_ts)
		
		  # for plotting
		  years <- df$year 
		  
  	# model
  	sem <-"
  	 # Link, lag, param_name
  	 
  	 # internal data relationship
		 mean_temp -> mean_temp, 1, ar_temp, 0.001
		 biomass -> biomass, 1, ar_bio, 0.001
  	 "

  	## fit model
  	fit = 
  	 dsem( 
  	  sem = sem,
  	  tsdata = df_ts) 
  
  	fit
  
	})
	
	saveRDS(dsem_models_no_temp, file = here("./output/all_dsem_mods_no_temp.RDS"))
  dsem_models_no_temp <- readRDS(file = here("./output/all_dsem_mods_no_temp.RDS"))
  
	#### function to fit dsem models WITH TEMPERATURE ####
	all_dsem_mod_fun <- purrr::safely(function(sp, lag) {

    df <- biomass_envr_dat |>
      filter(commonname == sp) |>
      select(commonname, year, value, mean_val) |>
      rename(
        biomass = value,
        mean_temp = mean_val
      ) |>
    	ungroup()

    df_ts <- df |>
      select(-year, -commonname) |>
      mutate(across(
        where(is.numeric),
        ~ (. - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
      ))

    df_ts <- ts(df_ts)

    sem <- glue::glue("
      mean_temp -> mean_temp, 1, ar_temp, 0.001
      biomass -> biomass, 1, ar_bio, 0.001
      mean_temp -> biomass, {lag}, t_b
    ")

    fit <- dsem(
      sem = sem,
      tsdata = df_ts)
    
    fit

})
  
  all_dsem_models <- purrr::map2(
  grid$species_list,
  grid$lags,
  all_dsem_mod_fun
)
	

	saveRDS(all_dsem_mods, file = here("./output/all_dsem_mods.RDS"))
 
	all_dsem_mods <- readRDS(file = here("./output/all_dsem_mods.RDS"))
	labels <- paste0(grid$species_list, "_", grid$lags)
	names(all_dsem_mods) <- labels
	
	#### compare AIC so can remove species where temperature does not affect biomass ####
	
	# calculate AICs
 
	sp_lags <- crossing(species_list, 0:25)
	spp <- sp_lags$species_list
	lags <- sp_lags$`0:25`
 
	# AIC of model with no temperature
  out_no_temp <- pmap_dfr(list(
 	 mod = dsem_models_no_temp,
   sp = spp),
   
      function(mod, sp) {

  				 val <- AIC(mod)

    			tab <- 
    				tibble(
    			  species = sp,
    			  val = val)
    			
    			tab
  })
 
  out_no_temp <- out_no_temp |>
  	distinct()
 
	# AIC of models with temperature
	out <- pmap_dfr(list(
		 mod = all_dsem_models,
	  sp = spp,
	  lag = lags),
   
      function(mod, sp, lag) {

  				 val <- AIC(mod$result)

    			tab <- 
    				tibble(
    			  species = sp,
    			  lag = lag,
    			  val = val)
    			
    			tab
  })
	
	out_no_temp$lag <- "no_temp"
	
	out$lag <- as.character(out$lag)
 
	all_AICs <- bind_rows(out, out_no_temp)
	
	best_mods <- all_AICs |>
		group_by(species) |>
  	filter(val <= min(val, na.rm = TRUE) + 2) |>
		ungroup()
	
	sp_drop <- best_mods |>
  	dplyr::filter(lag == "no_temp") |>
  	select(species)
	
	sp_drop <- as.vector(sp_drop$species)
	
	## remove 3 species unaffected by temp
	sp_keep <- setdiff(species_list, sp_drop)
 
	out <- out |>
		filter(species %in% sp_keep)
	
	out$lag <- as.numeric(out$lag)

	# plot 
 
	AIC_plot <-
		ggplot(data = out, aes(x = lag, y = val)) +
			geom_line(color = "#fff3b2") +
			ylab("AIC") + 
			xlab("Lag") +
			facet_wrap(~species, scales = "free", ncol = 5) +
			black_theme(z = 12, x = 12, y = 16)
 
	ggsave(AIC_plot, file = "./output/AIC_plot.png",
 			 height = 6, width = 12, units = "in")

	# coef plot
		slopes <- pmap_dfr(list(
 		mod = all_dsem_models,
  	sp = grid$species_list,
  	lag = grid$lags),
  	
  	   function(mod, sp, lag) {
	
  				est <- summary(mod$result)[3, 9] |> round(3)
  				se <- summary(mod$result)[3, 10] |> round(3)
  				
  				low_CI = est - (1.96*se)
  				high_CI = est + (1.96*se)
  				
  	 			tab <- 
  	 				tibble(
  	 			  species = sp,
  	 			  lag = lag,
  	 			  est = est,
  	 			  se = se,
  	 			  low_CI = low_CI,
  	 			  high_CI = high_CI)
  	 			
  	 			tab
  
  	})
	
	trim_AIC <- all_AICs |>
		filter(lag != "no_temp") |>
		filter(species %in% sp_keep) 
	
	trim_AIC$lag <- as.numeric(trim_AIC$lag)
	
	slopes <- left_join(slopes, trim_AIC)
	
	slopes_sum <- slopes |>
		rename(AIC = val) |>
		filter(species %in% sp_keep) |>
  	group_by(species) |>
		filter(AIC == min(AIC, na.rm = TRUE)) |>
  	ungroup()
  
 
coef_plot_dat <- slopes_sum |>
  mutate(species_lab = paste0(species, " (lag ", lag, ")"),
         species_lab = forcats::fct_reorder(species_lab, est))

coef_plot <- 
	ggplot(coef_plot_dat, aes(x = est, y = species_lab)) +
  	geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  	geom_errorbarh(aes(xmin = low_CI, xmax = high_CI), height = 0.2, color = "#fff3b2") +
  	geom_point(size = 2, color = "#fff3b2") +
  	labs(
  	  x = "Effect of temperature on biomass (slope)",
  	  y = NULL) +
  		black_theme(x = 10, y = 12)

 ggsave(coef_plot, file = "./output/coef_plot.png")
 
	
	
	
	best_mods <- all_AICs |>
		group_by(species) |>
  	filter(val <= min(val, na.rm = TRUE)) |>
		ungroup()
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	#####################################
	species_list <- unique(biomass_envr_dat$commonname)
	lags <- c(0:25)
	
	# df for function
	grid <- tidyr::expand_grid(
  	species_list = species_list,
  	lags = lags)
	

	
	all
	#######
	
	
	#### refit dsem models ####
	species_list <- unique(out$species)
	lags <- c(0:25)
	
	# df for function
	grid <- tidyr::expand_grid(
  	species_list = species_list,
  	lags = lags)
	

	final_dsem_models <- purrr::map2(grid$species_list, grid$lags, \(sp, lag){

		 # filter by species
		 df <- biomass_envr_dat |>
		 	filter(commonname == sp) |>
		 	select(commonname, year, value, mean_val) |>
		  rename(biomass = value,
		  			 mean_temp = mean_val)
		 
		 df_ts <- df |>
		 	select(-year, -commonname)
		 
		 df_ts <- df_ts |>
		 	mutate(across(where(is.numeric), ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)))
		
		  # turn into timeseries df
		  df_ts <- ts(df_ts)
		
		  # for plotting
		  years <- df$year 
		  
  	# model
  	sem <- glue("
  	 # Link, lag, param_name
  	 
  	 # internal data relationship
		 mean_temp -> mean_temp, 1, ar_temp, 0.001
		 biomass -> biomass, 1, ar_bio, 0.001
  	 
  	 # causal links
  	 mean_temp -> biomass, {lag}, t_b
  	 ")

  	## fit model
  tryCatch(
  	fit = 
  	 dsem( 
  	  sem = sem,
  	  tsdata = df_ts)
  
  	fit
  
	})
	
	saveRDS(final_dsem_models, file = here("./output/final_dsem_mods.RDS"))
 
	final_dsem_models <- readRDS(file = here("./output/final_dsem_models.RDS"))
	
	# pull out top models and collate slopes and SEs
	
		slopes <- pmap_dfr(list(
 		mod = final_dsem_models,
  	sp = grid$species_list,
  	lag = grid$lags),
  	
  	   function(mod, sp, lag) {
	
  				est <- summary(mod)[3, 9] |> round(3)
  				se <- summary(mod)[3, 10] |> round(3)
  				
  				low_CI = est - (1.96*se)
  				high_CI = est + (1.96*se)
  				
  	 			tab <- 
  	 				tibble(
  	 			  species = sp,
  	 			  lag = lag,
  	 			  est = est,
  	 			  se = se,
  	 			  low_CI = low_CI,
  	 			  high_CI = high_CI)
  	 			
  	 			tab
  
  	})
	
	trim_AIC <- all_AICs |>
		filter(lag != "no_temp") |>
		filter(species %in% sp_keep) 
	
	trim_AIC$lag <- as.numeric(trim_AIC$lag)
	
	slopes <- left_join(slopes, trim_AIC)
	
	slopes_sum <- slopes |>
		rename(AIC = val) |>
		filter(species %in% sp_keep) |>
  	group_by(species) |>
		filter(AIC == min(AIC, na.rm = TRUE)) |>
  	ungroup()
  
 
coef_plot_dat <- slopes_sum |>
  mutate(species_lab = paste0(species, " (lag ", lag, ")"),
         species_lab = forcats::fct_reorder(species_lab, est))

coef_plot <- 
	ggplot(coef_plot_dat, aes(x = est, y = species_lab)) +
  	geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  	geom_errorbarh(aes(xmin = low_CI, xmax = high_CI), height = 0.2, color = "#fff3b2") +
  	geom_point(size = 2, color = "#fff3b2") +
  	labs(
  	  x = "Effect of temperature on biomass (slope)",
  	  y = NULL) +
  		black_theme()

 ggsave(coef_plot, file = "./output/coef_plot.png",
 			 height = 8, width = 14, units = "in")
 
# for all low AIC
 
 slopes_all_top <- slopes |>
		rename(AIC = val) |>
		filter(species %in% sp_keep) |>
  	group_by(species) |>
  	filter(AIC <= min(AIC, na.rm = TRUE) + 2) |>
  	ungroup()
  
 
coef_plot_dat_top <- slopes_all_top |>
  mutate(species_lab = paste0(species, " (lag ", lag, ")"),
         species_lab = forcats::fct_reorder(species_lab, est))

coef_plot_top <- 
	ggplot(coef_plot_dat_top, aes(x = est, y = species_lab)) +
  	geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  	geom_errorbarh(aes(xmin = low_CI, xmax = high_CI), height = 0.2, 
  								 color = "#fff3b2") +
  	geom_point(size = 2, color = "#fff3b2") +
  	labs(
  	  x = "Effect of temperature on biomass (slope)",
  	  y = NULL) +
  		black_theme()

 ggsave(coef_plot_top, file = "./output/coef_plot_top.png",
 			 height = 8, width = 14, units = "in")
 