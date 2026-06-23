
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
	 