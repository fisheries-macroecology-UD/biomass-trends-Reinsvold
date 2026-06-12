

	#### no effect of temperature ####
	
	# function to fit dsem models
	dsem_models_no_temp_iid <- purrr::map(grid$species_list, \(sp){

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
  	sem <-"
  	 # Link, lag, param_name
  	 
  	 # internal data relationship
		 mean_temp -> mean_temp, 1, ar_temp, 0.001
		 biomass <-> biomass, 0, sigmaR, 1
  	 "

  	## fit model
  	fit = 
  	 dsem( 
  	  sem = sem,
  	  tsdata = df_ts) 
  
  	fit
  
	})
	
	
		# calculate AICs
 
	sp_lags <- crossing(species_list, 0:25)
	spp <- sp_lags$species_list
	lags <- sp_lags$`0:25`
 
	# AIC of model with no temperature
  out_no_temp_iid <- pmap_dfr(list(
 	 mod = dsem_models_no_temp_iid,
   sp = spp),
   
      function(mod, sp) {

  				 val <- AIC(mod)

    			tab <- 
    				tibble(
    			  species = sp,
    			  val = val)
    			
    			tab
  })
 
  out_no_temp_iid <- out_no_temp_iid |>
  	distinct()
  
  out_no_temp_iid <- out_no_temp_iid |>
  	rename(AIC_iid = val)
  
  out_no_temp <- out_no_temp |>
  	rename(AIC_ar1 = val)
  
  out_no_temp <- left_join(out_no_temp_iid, out_no_temp)
  