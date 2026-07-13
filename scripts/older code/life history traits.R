# life history traits 

	library(FishLife)

	# edge names
	edge_names = c( FishBase_and_Morphometrics$tree$tip.label,
                FishBase_and_Morphometrics$tree$node.label[-1] ) # Removing root
                
	names <- unique(biomass_envr_dat$scientificname)
	
	lh_traits <- purrr::map_dfr(names, \(sp) {
  
  which_g <- match(sp, edge_names)
  
  tab <- data.frame(
    trait = colnames(FishBase_and_Morphometrics$beta_gv),
    mean  = as.numeric(FishBase_and_Morphometrics$beta_gv[which_g, ]),
    name  = sp)
   
   tab

   
   })
	
	
	traits <- unique(lh_traits$trait)
	
	# subset list to traits we want to explore with respect to lagged temperature
	traits <- traits[-c(6, 13:20, 25:30)]
	
	lh_traits <- lh_traits |>
		filter(trait %in% traits)
	
	names_df <- biomass_envr_dat |>
		select(commonname, scientificname) |>
		distinct()
	
	lh_traits <- lh_traits |>
		rename(scientificname = name)
	
	lh_traits <-left_join(lh_traits, names_df)
	
	lh_traits$mean <- exp(lh_traits$mean)
	