
# 01 - download biomass data from StockSmart

	# install the R package
	# remotes::install_github("NOAA-EDAB/stocksmart")

	library(tidyverse)
	library(stocksmart)
	library(here)
	library(dsem)
	
	`%!in%` <- Negate(`%in%`)
	
	#### 1. download biomass data from stock smart using the R package ####

  
  # abundance data

		# pull all output from the latest full assessment for each stock
    biomass_dat <- get_latest_full_assessment()
    
    # the result is a list of the data and a summary, we want to pull the data 
    biomass_dat <- biomass_dat$data
    
    # look at all columns 
		glimpse(biomass_dat)
		
		# how many stocks are included
		length(unique(biomass_dat$itis))
		
	# missing data
	biomass_dat |> 
  		summarise(across(everything(), ~sum(is.na(.)))) |> 
  		pivot_longer(everything(), names_to = "column", values_to = "n_na") |> 
  		filter(n_na > 0)
	
	NAs <- biomass_dat |>
		filter(is.na(common_name))
	
	## looks like ~3500 rows have NAs for names, many of these are species complexes
	## there is also black grouper but its data is not separated between GOM and SA so do not retain
	
	biomass_dat <- biomass_dat |>
			select(stock_name, common_name, stock_area,
  				 assessment_year, regional_ecosystem,
					 year, value, description, units, 
					 scientific_name, jurisdiction,
					 assessment_type) |>
			drop_na()
		

	ecosystems <- unique(biomass_dat$regional_ecosystem)
		
	reg <- grep("Northeast|Alaska|Cal|Southeast|Gulf", ecosystems, value = TRUE)		
	
	biomass_dat <- biomass_dat |>
		filter(regional_ecosystem %in% reg)

	# fix Northern rockfish
		
	nrf <- biomass_dat |>
		filter(common_name == "Northern rockfish")
	
	nrf$value[nrf$value == 48630.00] <- 148630.00
	
	biomass_dat <- biomass_dat |>
		filter(common_name != "Northern rockfish")
	
	biomass_dat <- bind_rows(biomass_dat, nrf)

	unique(biomass_dat$description)
	
	# lots of different units of 'biomass' - need to figure out which one(s) to use
	
	# first step - available metrics in each area
	
	sum_stocks <- biomass_dat |>
		group_by(regional_ecosystem) |>
		summarise(n_distinct(stock_name))
	
	tmp <- biomass_dat |>
		filter(grepl("Biomass", description)) |>
		group_by(regional_ecosystem, stock_name) |>
		summarise(biomass_type = str_flatten_comma(unique(description)), 
    .groups = "drop"
  )
	
	unique(tmp$description)
	
	biomass_dat <- biomass_dat |>
		filter(grepl("Biomass", description))

	# we want to remove any metrics of biomass that differ largely from SSB
	biomass_metrics <- unique(biomass_dat$description)
		
	remove <- grep("Catch|catch|0|Exploit", biomass_metrics, value = TRUE)		
	
	biomass_dat <- biomass_dat |>
		filter(description %!in% remove)

	"Red king crab - Norton Sound" %in% biomass_dat$stock_name
	
	biomass_dat |>
		group_by(regional_ecosystem) |>
		summarise(cont = n_distinct(stock_name))

	# create new column of subregion to separate out Western stocks 
	biomass_dat <- biomass_dat |>
  	mutate(
    	subregion = case_when(
    	  regional_ecosystem == "California Current" ~ "California Current",
    	  regional_ecosystem == "Gulf of Mexico" ~ "Gulf of Mexico",
    	  regional_ecosystem == "Northeast Shelf" ~ "Northeast Shelf",
    	  regional_ecosystem == "Southeast Shelf" ~ "Southeast Shelf",
    	  regional_ecosystem == "Alaska Ecosystem Complex" &
        	str_detect(stock_area, regex("Gulf", ignore_case = TRUE)) ~ "Gulf of Alaska",
				regional_ecosystem == "Alaska Ecosystem Complex" ~ "Bering Sea"))
	
	# add Sablefish back to Bering Sea - biomass is split between subregions
	biomass_dat <- bind_rows(
	  biomass_dat,
	  biomass_dat |>
	    filter(common_name == "Sablefish", subregion == "Gulf of Alaska") |>
	    mutate(subregion = "Bering Sea")
	)
	
	tmp <- biomass_dat |>
	  distinct(subregion, common_name, description, stock_area) |>
	  group_by(subregion, common_name, stock_area) |>
	  summarise(
	    desc_stocks = paste0(
	      description[1], " (",
	      paste(sort(unique(stock_area)), collapse = "; "), ")"
	    ),
	    .groups = "drop_last"
	  ) |>
	  summarise(
	    n = n_distinct(stock_area),
	    descriptions = paste(desc_stocks, collapse = " | "),
	    .groups = "drop"
	  ) |>
	  filter(n >= 2)
	
	# clearing up irrelevant stock metrics
	biomass_dat <- biomass_dat |>
	  # remove Hogfish age 1 biomass metric
	  filter(!(common_name == "Hogfish" & description == "Biomass Age 1")) |>
	  
	  # remove Cabezon California "mean" biomass indicator
	  filter(!(common_name == "Cabezon" & description == "Female Mature Biomass (Mean)")) |>
	  
	  # remove Atlantic cod, Western Gulf of Maine stock - eastern counterpart
	  # not available; using GOM predecessor indicator for the whole region instead
	  # also remove Eastern Georges Bank subset
	  filter(!(common_name == "Atlantic cod" & stock_area == "Western Gulf of Maine")) |>
	  filter(!(common_name == "Atlantic cod" & stock_area == "Eastern Georges Bank")) |>
	
	  # remove Black rockfish California stock - switched to state monitoring in 2017
	  filter(!(common_name == "Black rockfish" & description == "Female Mature Biomass (Mean)")) |>
	
	  #remove Haddock eastern Geoerge Bank subset
	  filter(!(common_name == "Haddock" & stock_area == "Eastern Georges Bank")) |>
	
	  #remove Winter flounder Gulf of Maine stock, no documentation reflecting spawning biomass calculations
	  filter(!(common_name == "Winter flounder" & description == "Spawning Stock Biomass, Age 1")) |>
	
	  #remove Rex sole regional stocks - Gulf of Alaska already sums the other two stocks
    filter(!(common_name == "Rex sole" & stock_area %in% c("Eastern Gulf of Alaska", "Western / Central Gulf of Alaska")))
	  
	
  # convert all units to metric tons
	biomass_dat <- biomass_dat |>
	  mutate(
	    value = dplyr::case_when(
	      units %in% c("Metric Tons", "mt")   ~ value,
	      units == "Thousand Metric Tons"     ~ value * 1000,
	      units == "Million Pounds"           ~ value * 453.592,
	      TRUE                                ~ value
	    ),
	    units = "Metric Tons"
	  )
	 
	# make table to show stocks with 2+ biomass metrics, in the same region
	# shows stocks for which metrics/regions were added together
	stock_summation <- biomass_dat |>
	  distinct(subregion, common_name, description, stock_area) |>
	  group_by(subregion, common_name, stock_area) |>
	  summarise(
	    desc_stocks = paste0(
	      description[1], " (",
	      paste(sort(unique(stock_area)), collapse = "; "), ")"
	    ),
	    .groups = "drop_last"
	  ) |>
	  summarise(
	    n = n_distinct(stock_area),
	    descriptions = paste(desc_stocks, collapse = " | "),
	    .groups = "drop"
	  ) |>
	  filter(n >= 2)
	
	# sum SSB across stocks within a subregion; keep single-stock species as-is
	biomass_dat <- biomass_dat |>
	  group_by(subregion, common_name) |>
	  mutate(n_stocks = n_distinct(stock_area)) |>
	  # for multi-stock species, only keep years where all stocks report
	  group_by(subregion, common_name, year) |>
	  filter(n_distinct(stock_area) == first(n_stocks)) |>
	  group_by(subregion, common_name, year) |>
	  summarise(
	    value           = sum(value),
	    n_stocks        = first(n_stocks),
	    stock_name      = if (first(n_stocks) >= 2)
	      paste(first(common_name), first(subregion))
	    else first(stock_name),
	    stock_area      = if (first(n_stocks) >= 2)
	      first(subregion)
	    else first(stock_area),
	    assessment_year = max(assessment_year),
	    regional_ecosystem = first(regional_ecosystem),
	    description     = "Spawning Stock Biomass",
	    units           = "Metric Tons",
	    scientific_name = first(scientific_name),
	    jurisdiction    = first(jurisdiction),
	    assessment_type = first(assessment_type),
	    .groups = "drop"
	  )
	
	# filter west coast species to specific subregions
	reg <- grep("Alaska|Bering|Current", biomass_dat$subregion, value = TRUE)
	biomass_dat <- biomass_dat |>
	  filter(subregion %in% reg)
