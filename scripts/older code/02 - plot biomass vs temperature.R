#  plot biomass vs temperature
  
	# black theme
	black_theme <- function(x = 12, y = 14, z = 16) {
  	theme(legend.position = "none",
  				axis.text = element_text(size = x, colour = "grey"),
  				axis.title = element_text(size = y, color = "white"),
  				axis.line = element_line(color = "grey", linewidth = 1),
  				axis.ticks = element_line(colour = "grey"),
  				legend.title = element_text(color = "white"),
  				legend.text = element_text(color = "white"),
  				panel.background = element_rect(fill = "black"),
					panel.grid = element_blank(),
					panel.border =element_rect(fill = NA),
					strip.background = element_rect(fill = "black", color = "black"),
					strip.text = element_text(color = "white", size = z),
  				plot.background = element_rect(fill = "black", color = "black"))
		}
		

  # function to plot temp and biomass for all species
  
  species <- unique(biomass_envr_dat$commonname)
  
  plots <-
  	purrr::map(species, \(sp){ 
  
  	nd <- biomass_envr_dat |>
  		filter(commonname == sp) |>
  		mutate()
  	
  	nd <- nd %>%
  		mutate(
  	  	mean_val_scaled =
  	  	  (mean_val - min(mean_val, na.rm = TRUE)) /
  	  	  (max(mean_val, na.rm = TRUE) - min(mean_val, na.rm = TRUE)) *
  	  	  (max(value, na.rm = TRUE) - min(value, na.rm = TRUE)) +
  	  	  min(value, na.rm = TRUE))
  	
  	nd_names <- nd |>
  		rename(SSB = value,
  					 temp = mean_val_scaled)
	
		p <- 
			ggplot(nd_names, aes(x = year)) +
		  geom_line(aes(y = SSB), color = "blue", linewidth = 1) +
			geom_point(aes(y = SSB), color = "blue") +
		  geom_line(aes(y = temp), color = "red", linewidth = 1) +
		  labs(y = "Biomass (blue), Temperature (scaled red)") +
			ggtitle(sp) +
			ggsidekick::theme_sleek() + 
			black_theme() +
			theme(legend.position = "right")
			
		
  })
		
  
 plots 
  
 # scale temperature by biomass for each group
 biomass_envr_dat <- biomass_envr_dat |>
 	group_by(commonname) |>
 		mutate(
  	  	mean_val_scaled =
  	  	  (mean_val - min(mean_val, na.rm = TRUE)) /
  	  	  (max(mean_val, na.rm = TRUE) - min(mean_val, na.rm = TRUE)) *
  	  	  (max(value, na.rm = TRUE) - min(value, na.rm = TRUE)) +
  	  	  min(value, na.rm = TRUE))
 
 	biomass_envr_dat_plot <- biomass_envr_dat |>
  		rename(SSB = value,
  					 temp = mean_val_scaled)
	
 

 	#### faceted biomass no temperature ####
 	
 	
 	biomass_trends_plot <- 
			ggplot(biomass_envr_dat_plot, aes(x = year)) +
 			facet_wrap(~ commonname, scales = "free", ncol = 6) +
		  geom_line(aes(y = SSB), alpha = 0.5, color = "#fff3b2", linewidth = 1) +
			geom_point(aes(y = SSB), alpha = 0.5, color = "#fff3b2", size = 0.5) +
 			labs(y = expression("Biomass (x "*10^5*" metric tons)"), x = "Year") +
 			scale_y_continuous(labels = scales::label_number(scale = 1e-5)) +
			black_theme(x = 16, y = 16) 

 	
 	ggsave(here("./output/biomass_figure.png"),
 				 height = 12, width = 20)
 	
 	#### biomass with temperature ####
 	
 		p <- 
			ggplot(biomass_envr_dat_plot, aes(x = year)) +
 			facet_wrap(~ commonname, scales = "free", ncol = 6) +
		  geom_line(aes(y = SSB), alpha = 0.5, color = "#fff3b2", linewidth = 1) +
			geom_point(aes(y = SSB), alpha = 0.5, color = "#fff3b2", size = 0.5) +
		  geom_line(aes(y = temp), alpha = 0.5, color = "#f2f2f2", linewidth = 0.5) +
 			labs(y = expression("Biomass (x "*10^5*"), Temperature (scaled, grey)")) +
 			scale_y_continuous(labels = scales::label_number(scale = 1e-5)) +
			black_theme(x = 16, y = 16) 
 	
 	
 	ggsave(here("./output/biomass_temp_figure.png"),
 				 height = 10, width = 20)
 	
 