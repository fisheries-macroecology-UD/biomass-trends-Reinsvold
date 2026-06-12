# 06 - DSEM

 # filter by Atka mackerel
 atka <- biomass_envr_dat |>
 	filter(commonname == "Atka mackerel") |>
 	select(commonname, year, value, mean_val) |>
  rename(biomass = value,
  			 mean_temp = mean_val)
 
 atka_ts <- atka |>
 	select(-year, -commonname)
 
 atka_ts <- atka_ts |>
 	mutate(across(where(is.numeric), ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)))

  # turn into timeseries df
  atka_ts <- ts(atka_ts)

  # for plotting
  years <- atka$year 
  

	# dagitty code

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
    tsdata = atka_ts) 
  
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
    tsdata = atka_ts) 
  
  AIC(fit1)
  
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
    tsdata = atka_ts) 
  
  AIC(fit2)
  
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
    tsdata = atka_ts) 
  
  AIC(fit3)
  
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
    tsdata = atka_ts) 
  
  AIC(fit4)
  
  
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
    tsdata = atka_ts) 
  
  AIC(fit5)
  
  
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
    tsdata = atka_ts) 
  
  AIC(fit6)
 
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
    tsdata = atka_ts) 
  
  AIC(fit7)
  
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
    tsdata = atka_ts) 
  
  AIC(fit8)
  
    
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
    tsdata = atka_ts) 
  
  AIC(fit9)
  
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
    tsdata = atka_ts) 
  
  AIC(fit10)
  
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
    tsdata = atka_ts) 
  
  AIC
  
    
  # 12 yr lag #
  
  sem12 <- "
   # Link, lag, param_name
   
   # internal data relationship
	 mean_temp -> mean_temp, 1, ar_temp, 0.001
	 biomass -> biomass, 1, ar_bio, 0.001
   
   # causal links
   mean_temp -> biomass, 12, t_b
  "

  ## remove insignificant links? (condition, nearshore age1, eke)
  ## fit model
  fit12 = 
   dsem( 
    sem = sem12,
    tsdata = atka_ts) 
  
  AIC(fit12)
  
  # 15 
  
   
  sem15 <- "
   # Link, lag, param_name
   
   # internal data relationship
	 mean_temp -> mean_temp, 1, ar_temp, 0.001
	 biomass -> biomass, 1, ar_bio, 0.001
   
   # causal links
   mean_temp -> biomass, 15, t_b
  "

  ## remove insignificant links? (condition, nearshore age1, eke)
  ## fit model
  fit15 = 
   dsem( 
    sem = sem15,
    tsdata = atka_ts) 
  
 AIC(fit15)
  
  
  # make AIC table 
 
 mod_list <- mget(ls(pattern = "^fit[0-9]+$"), envir = .GlobalEnv)
 
 mod_names <- names(mod_list)

  out <- purrr::map2_dfr(mod_list, mod_names, \(mod, name){
		
		species <- unique(atka$commonname)

		val <- AIC(mod)
		
		lag <- as.numeric(str_extract(name, "\\d+"))
		
		tab <- tibble(species, lag, val)
		
		tab
		
  })
  
  
	#	table_SX <- tab |> gt::gt()
	
	#gt::gtsave(table_SX, file = paste0(here(), "/output/tables/table_sample_size.pdf"))
	#gt::gtsave(table_SX, file = paste0(here(), "/output/tables/table_sample_size.png"))

  
  
  ##################
  
  AIC(fit0)
  AIC(fit1)
  AIC(fit2)
  AIC(fit3)
  AIC(fit4)
  AIC(fit5)
  AIC(fit6)
  AIC(fit7)
  AIC(fit8)
  AIC(fit9)
  AIC(fit10)
  AIC(fit11)
	AIC(fit12)  
	
	