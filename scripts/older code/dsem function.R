# function to fit DSEM model to other species

# 06 - DSEM

	species_list <- unique(biomass_EBS$commonname)

	dsem_models <- purrr::map(species_list, \(sp){

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
		  



  #### no lag ####
  sem0 <- "
   # Link, lag, param_name
   
   # internal data relationship
	 mean_temp -> mean_temp, 1, ar_temp, 0.001
	 biomass -> biomass, 1, ar_bio, 0.001
   
   # causal links
   mean_temp -> biomass, 0, t_b
  "

  ## fit model
  fit0 = 
   dsem( 
    sem = sem0,
    tsdata = df_ts) 
  
  AIC(fit0)
  
 #### 1yr lag ####

 sem1 <- "
   # Link, lag, param_name
   
   # internal data relationship
	 mean_temp -> mean_temp, 1, ar_temp, 0.001
	 biomass -> biomass, 1, ar_bio, 0.001
   
   # causal links
   mean_temp -> biomass, 1, t_b
  "

  ## fit model
  fit1 = 
   dsem( 
    sem = sem1,
    tsdata = df_ts) 
  
  # 2 yr lag
  sem2 <- "
   # Link, lag, param_name
   
   # internal data relationship
	 mean_temp -> mean_temp, 1, ar_temp, 0.001
	 biomass -> biomass, 1, ar_bio, 0.001
   
   # causal links
   mean_temp -> biomass, 2, t_b
  "

    ## fit model
  fit2 = 
   dsem( 
    sem = sem2,
    tsdata = df_ts) 
  

  #### 3yr lag ####

  sem3 <- "
   # Link, lag, param_name
   
   # internal data relationship
	 mean_temp -> mean_temp, 1, ar_temp, 0.001
	 biomass -> biomass, 1, ar_bio, 0.001
   
   # causal links
   mean_temp -> biomass, 3, t_b
  "

  ## fit model
  fit3 = 
   dsem( 
    sem = sem3,
    tsdata = df_ts) 
  

 #### 4 yr lag ####
  
  sem4 <- "
   # Link, lag, param_name
   
   # internal data relationship
	 mean_temp -> mean_temp, 1, ar_temp, 0.001
	 biomass -> biomass, 1, ar_bio, 0.001
   
   # causal links
   mean_temp -> biomass, 4, t_b
  "

  ## remove insignificant links? (condition, nearshore age1, eke)
  ## fit model
  fit4 = 
   dsem( 
    sem = sem4,
    tsdata = df_ts) 
  

  
  #### 5 yr lag ####
  sem5 <- "
   # Link, lag, param_name
   
   # internal data relationship
	 mean_temp -> mean_temp, 1, ar_temp, 0.001
	 biomass -> biomass, 1, ar_bio, 0.001
   
   # causal links
   mean_temp -> biomass, 5, t_b
  "

  ## fit model
  fit5 = 
   dsem( 
    sem = sem5,
    tsdata = df_ts) 
  

  
  #### 6 yr lag ####
  
  sem6 <- "
   # Link, lag, param_name
   
   # internal data relationship
	 mean_temp -> mean_temp, 1, ar_temp, 0.001
	 biomass -> biomass, 1, ar_bio, 0.001
   
   # causal links
   mean_temp -> biomass, 6, t_b
  "

  ## fit model
  fit6 = 
   dsem( 
    sem = sem6,
    tsdata = df_ts) 
  

  #### 7 yr lag ####
  sem7 <- "
   # Link, lag, param_name
   
   # internal data relationship
	 mean_temp -> mean_temp, 1, ar_temp, 0.001
	 biomass -> biomass, 1, ar_bio, 0.001
   
   # causal links
   mean_temp -> biomass, 7, t_b
  "

  ## fit model
  fit7 = 
   dsem( 
    sem = sem7,
    tsdata = df_ts) 
  

    #### 8 yr lag ####
  sem8 <- "
   # Link, lag, param_name
   
   # internal data relationship
	 mean_temp -> mean_temp, 1, ar_temp, 0.001
	 biomass -> biomass, 1, ar_bio, 0.001
   
   # causal links
   mean_temp -> biomass, 8, t_b
  "

  ## fit model
  fit8 = 
   dsem( 
    sem = sem8,
    tsdata = df_ts) 
  

    
    #### 9 yr lag ####
  sem9 <- "
   # Link, lag, param_name
   
   # internal data relationship
	 mean_temp -> mean_temp, 1, ar_temp, 0.001
	 biomass -> biomass, 1, ar_bio, 0.001
   
   # causal links
   mean_temp -> biomass, 9, t_b
  "

  ## fit model
  fit9 = 
   dsem( 
    sem = sem9,
    tsdata = df_ts) 
  

  # 10 yr lag #
  
  sem10 <- "
   # Link, lag, param_name
   
   # internal data relationship
	 mean_temp -> mean_temp, 1, ar_temp, 0.001
	 biomass -> biomass, 1, ar_bio, 0.001
   
   # causal links
   mean_temp -> biomass, 10, t_b
  "

  ## remove insignificant links? (condition, nearshore age1, eke)
  ## fit model
  fit10 = 
   dsem( 
    sem = sem10,
    tsdata = df_ts) 
  

  # 11 yr lag #
  
  sem11 <- "
   # Link, lag, param_name
   
   # internal data relationship
	 mean_temp -> mean_temp, 1, ar_temp, 0.001
	 biomass -> biomass, 1, ar_bio, 0.001
   
   # causal links
   mean_temp -> biomass, 11, t_b
  "

  ## remove insignificant links? (condition, nearshore age1, eke)
  ## fit model
  fit11 = 
   dsem( 
    sem = sem11,
    tsdata = df_ts) 
  

  # 12 yr lag #
  
  sem12 <- "
   # Link, lag, param_name
   
   # internal data relationship
	 mean_temp -> mean_temp, 1, ar_temp, 0.001
	 biomass -> biomass, 1, ar_bio, 0.001
   
   # causal links
   mean_temp -> biomass, 12, t_b
  "

  ## fit model
  fit12 = 
   dsem( 
    sem = sem12,
    tsdata = df_ts) 
  
  
  # 13 yr lag #
  
  sem13 <- "
   # Link, lag, param_name
   
   # internal data relationship
	 mean_temp -> mean_temp, 1, ar_temp, 0.001
	 biomass -> biomass, 1, ar_bio, 0.001
   
   # causal links
   mean_temp -> biomass, 13, t_b
  "

  ## fit model
  fit13 = 
   dsem( 
    sem = sem13,
    tsdata = df_ts) 
  
  # 14 yr lag #

sem14 <- "
 # Link, lag, param_name
 
 # internal data relationship
 mean_temp -> mean_temp, 1, ar_temp, 0.001
 biomass -> biomass, 1, ar_bio, 0.001
 
 # causal links
 mean_temp -> biomass, 14, t_b
"

fit14 <- dsem(
  sem = sem14,
  tsdata = df_ts
)


# 15 yr lag #

sem15 <- "
 # Link, lag, param_name
 
 # internal data relationship
 mean_temp -> mean_temp, 1, ar_temp, 0.001
 biomass -> biomass, 1, ar_bio, 0.001
 
 # causal links
 mean_temp -> biomass, 15, t_b
"

fit15 <- dsem(
  sem = sem15,
  tsdata = df_ts
)


# 16 yr lag #

sem16 <- "
 # Link, lag, param_name
 
 # internal data relationship
 mean_temp -> mean_temp, 1, ar_temp, 0.001
 biomass -> biomass, 1, ar_bio, 0.001
 
 # causal links
 mean_temp -> biomass, 16, t_b
"

fit16 <- dsem(
  sem = sem16,
  tsdata = df_ts
)


# 17 yr lag #

sem17 <- "
 # Link, lag, param_name
 
 # internal data relationship
 mean_temp -> mean_temp, 1, ar_temp, 0.001
 biomass -> biomass, 1, ar_bio, 0.001
 
 # causal links
 mean_temp -> biomass, 17, t_b
"

fit17 <- dsem(
  sem = sem17,
  tsdata = df_ts
)


# 18 yr lag #

sem18 <- "
 # Link, lag, param_name
 
 # internal data relationship
 mean_temp -> mean_temp, 1, ar_temp, 0.001
 biomass -> biomass, 1, ar_bio, 0.001
 
 # causal links
 mean_temp -> biomass, 18, t_b
"

fit18 <- dsem(
  sem = sem18,
  tsdata = df_ts
)


# 19 yr lag #

sem19 <- "
 # Link, lag, param_name
 
 # internal data relationship
 mean_temp -> mean_temp, 1, ar_temp, 0.001
 biomass -> biomass, 1, ar_bio, 0.001
 
 # causal links
 mean_temp -> biomass, 19, t_b
"

fit19 <- dsem(
  sem = sem19,
  tsdata = df_ts
)


# 20 yr lag #

sem20 <- "
 # Link, lag, param_name
 
 # internal data relationship
 mean_temp -> mean_temp, 1, ar_temp, 0.001
 biomass -> biomass, 1, ar_bio, 0.001
 
 # causal links
 mean_temp -> biomass, 20, t_b
"

fit20 <- dsem(
  sem = sem20,
  tsdata = df_ts
)
  
  
  mod_list <- list(fit0,
  								 fit1,
  								 fit2,
  								 fit3,
  								 fit4,
  								 fit5,
  								 fit6,
  								 fit7,
  								 fit8,
  								 fit9,
  								 fit10,
  								 fit11,
  								 fit12,
  								 fit13,
  								 fit14,
  								 fit15,
  								 fit16,
  								 fit17,
  								 fit18,
  								 fit19,
  								 fit20)
 

  mod_list
		
  })
  

  
  # make AIC table 
 
 all_mod_list <- flatten(dsem_models)
 
 saveRDS(all_mod_list, file = here("./output/all_dsem_mods.RDS"))
 
 sp_lags <- crossing(species_list, 0:20)

 spp <- sp_lags$species_list
 
 lags <- sp_lags$`0:20`
 


 out <- pmap_dfr(list(
 	 mod = all_dsem_mods,
   sp = spp,
   lag = lags),
   
      function(mod, sp, lag) {

  				 val <- AIC(mod)

    			tab <- 
    				tibble(
    			  species = sp,
    			  lag = lag,
    			  val = val)
    			
    			tab
  })
 
 
 # plot 
 
 ggplot(data = out, aes(x = lag, y = val)) +
 	geom_line() +
 	facet_wrap(~species, scales = "free")
 
 
 # pull out most supported lag for each species, which means that temperature affects biomass X years later
 
 out_sum <- out |>
  group_by(species) |>
  mutate(delta_aic = val - min(val, na.rm = TRUE)) |>
  filter(delta_aic <= 2) |>
  ungroup()

 clear_sp <- out_sum |>
  group_by(species) |>
  filter(n() == 1) |>
  ungroup()
 
 # pull out slopes 

 slopes <- pmap_dfr(list(
 	 mod = all_mod_list,
   sp = spp,
   lag = lags),
   
      function(mod, sp, lag) {

  			est <- summary(mod)[3, 9]
  			est <- round(est, digits = 3)
  			se <- summary(mod)[3, 10]
  			se <- round(se, digits = 3)
  			
  			val <- AIC(mod)

    			tab <- 
    				tibble(
    			  species = sp,
    			  lag = lag,
    			  AIC = val,
    			  est = est,
    			  se = se)
    			
    			tab
  })
 
 
  slopes_sum <- slopes |>
  	group_by(species) |>
  	mutate(delta_aic = AIC - min(AIC, na.rm = TRUE)) |>
  	filter(delta_aic <= 2) |>
  	ungroup()

	all_names <- paste0(spp, "_", lag)
	names(all_mod_list) <- all_names
	
  subset_spp <- slopes_sum$species
  subset_lag <- slopes_sum$lag
  
  subset_mod_names <- paste0(subset_spp, "_", subset_lag)

  mod_subset <- all_mod_list[names(all_mod_list) %in% subset_mod_names]
  