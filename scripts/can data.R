# read in Canadian biomass data

  library(here)
  library(tidyverse)
  library(bayesdfa)
  
  # load data 
  can_dat <- read_rds(here("b-status-dat.rds"))

  glimpse(can_dat)  
  