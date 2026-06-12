
# 01 - download biomass data from StockSmart

	# install the R package
	# remotes::install_github("NOAA-EDAB/stocksmart")

	library(tidyverse)
	library(stocksmart)
	library(here)
	library(dsem)
	
	#### 1. download biomass data from stock smart using the R package ####

  
  # abundance data

    biomass_dat <- get_latest_metrics(metrics = "Abundance")
    biomass_dat <- biomass_dat$data

		glimpse(biomass_dat)
		length(unique(biomass_dat$ITIS))
		
		biomass_dat <- biomass_dat |>
			select(StockName, CommonName, StockArea,
  				 AssessmentYear, Year, Value, Description,
  				 Units, ScientificName, Jurisdiction) |>
			drop_na()
		
		unique(biomass_dat$Jurisdiction)
		
	# fix Northern rockfish
		
	nrf <- biomass_dat |>
		filter(CommonName == "Northern rockfish")
	
	nrf$Value[nrf$Value == 48630.00] <- 148630.00
	
	biomass_dat <- biomass_dat |>
		filter(CommonName != "Northern rockfish")
	
	biomass_dat <- bind_rows(biomass_dat, nrf)


	biomass_EBS <- biomass_dat |>
  	filter(str_detect(StockArea, "Bering Sea")) |>
  	select(StockName, CommonName, StockArea,
  				 AssessmentYear, Year, Value, Description,
  				 ScientificName, Units)
 
	 unique(biomass_EBS$CommonName)
	 
	 names(biomass_EBS) <- tolower(names(biomass_EBS))
	 
	# remove metrics other than SSB
	 biomass_EBS <- biomass_EBS |>
	 	filter(description != "Survey-Estimated Biomass")
 
	 # add temperature data to biomass data
	 
	# read in yearly-averaged bottom temperature from Bering10K
  btemp <- read_csv("./data/yearly_btemp_B10K.csv")
  
  # merge data sets
  
  biomass_envr_dat <- left_join(biomass_EBS, btemp)

  # black theme
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
	 