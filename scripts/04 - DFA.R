# DFA - looking for similar trends in biomass of groundfishes in EBS

	library(bayesdfa)

	# get data set up
	
	# filter out stocks with fewer than 5 years of data
	dat_dfa <- biomass_EBS %>%   
		select(commonname, year, value) |>
    group_by(commonname) %>%
	  filter(n() >= 5) |>
		ungroup()

 dat_dfa <- dat_dfa |>
 				arrange(commonname, year) |>
        mutate(time = year - min(year) + 1) |>
        arrange(commonname, time) |>
        rename(obs = value, ts = commonname) |>
 				select(-year) |>
        ungroup()

  # set seed 
  set.seed(682)
  
  # fit model with one trend
  fit_long <- fit_dfa(
    y = dat_dfa, 
    num_trends = 1, 
    scale="zscore",
    iter = 10000, 
    chains = 1, 
    thin = 1,
    data_shape = "long")
   
  # save
  saveRDS(fit_long, file = "./output/fit_long.rds")
 
  fit_long <- readRDS( file = "./output/fit_long.rds") 
  
  # check convergence
  is_converged(fit, threshold = 1.05)
  
  # plot
  r <- rotate_trends(fit_long)
  
  plot_trends(r) + theme_bw()
  
# plot_fitted(fit) + theme_bw()
  
  plot_loadings(r) + theme_bw()
  
	#### plots ####
   
  ## trend plot ##
  yrs <- sort(unique(biomass_envr_dat$year))
  
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
	
	spp_names <- unique(fit_long$orig_data$ts)
	
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
	 	#	scale_fill_manual(values = c(
	 	#		"Trend 1" = "#fff3b2")) +
	 	#	scale_color_manual(values = c(
	 	#		"Trend 1" = "#fff3b2")) +
      coord_flip() +
      xlab("Species") +
      ylab("Loading") +
	 	black_theme(x = 20, y = 16) +
	 	theme(panel.grid.major = element_line("#323232"))
	 
	 	ggsave(loadings, file = "./output/loadings_plot.png",
	 			 height = 10, width = 10, units = "in")
	
  #### plot against temperature ####
	 
	sp_trends <- t(r$trends_mean)
	
	df <- data.frame("sp_trend" = sp_trends, "year" = yrs) |>
		rename(trend = X1)
	
	df <- left_join(df, btemp) |>
		select(trend, year, mean_val)
	
	df_trim <- df |> drop_na(mean_val)
	cor(df_trim)
	 	
	 ggplot(df, aes(x = mean_val, y = trend)) +
	 	geom_point() +
	 	geom_text_repel(aes(label=year),size=3,color="gray60")
	 	
	 	
	 	
	 	
	 	
	 	
	 	
	 	
	 	#### without species that do not have an effect of temp
	 

	# get data set up
  dat <- biomass_envr_dat |>
      mutate(time = year - min(year) + 1) |>
      arrange(time) |>
      rename(obs = value, ts = commonname) |>
      ungroup()
  
  dat_trim <- dat |>
  	filter(ts %in% sp_keep)

  # set seed 
  set.seed(682)
  
  # fit model
  fit <- fit_dfa(
    y = dat_trim, 
    num_trends = 1, 
    scale="zscore",
    iter = 10000, 
    chains = 1, 
    thin = 1,
    data_shape = "long")
   
  # check convergence
  is_converged(fit, threshold = 1.05)
  
  # plot
  r <- rotate_trends(fit)
  
  plot_trends(r) + theme_bw()
  
# plot_fitted(fit) + theme_bw()
  
  plot_loadings(r) + theme_bw()
  
	spp_names <- unique(dat_trim$ts)
	
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
	 								fill = trend, color = trend, alpha = prob_diff0)) +
	 		scale_fill_manual(values = c(
	 			"Trend 1" = "#fff3b2")) +
	 		scale_color_manual(values = c(
	 			"Trend 1" = "#fff3b2")) +
      coord_flip() +
      xlab("Time Series") +
      ylab("Loading") +
	 	black_theme() +
	 	theme(panel.grid.major = element_line("#323232"))
	
	 plot_fitted(fit)
	 