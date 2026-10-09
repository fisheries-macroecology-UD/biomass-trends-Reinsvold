  library(tidyverse)
  library(ncdf4)
  library(sf)
  library(lubridate)
  library(terra)
  library(here)
  
  # load Alaska and West Coast data
  ak_wc_data <- readRDS(
    here("data for maps",
         "biol_haul_species_surveyjoin_12Nov2025.rds")) |>
    transmute(
      longitude = lon_start,
      latitude  = lat_start,
      survey_id) |>
    drop_na() |>
    distinct()
  
  # trim data to specific regions
  
  # Gulf of Alaska
  goa_data <- ak_wc_data |>
    filter(survey_id == "AFSC GOA")
  
  # Bering Sea
  ebs_regions <- c(
    "AFSC EBS",
    "AFSC AI",
    "AFSC BSS",
    "AFSC NBS")
  
  ebs_data <- ak_wc_data |>
    filter(survey_id %in% ebs_regions)
  
  # California Current
  cc_data <- ak_wc_data |>
    filter(survey_id %in% c(
      "NWFSC Combo",
      "NWFSC Slope"))
  
  # load northeast data
  ne_data <- read_csv(
    here("data for maps", "survdat_datapull.csv"),
    show_col_types = FALSE) |>
    rename_with(tolower) |>
    transmute(
      latitude = lat,
      longitude = lon) |>
    drop_na() |>
    distinct()
  
  # function to make a mask for each region
  
  make_mask <- function(df, projected_crs, ratio = 0.05) {
  
    df |>
      distinct(longitude, latitude) |>
      st_as_sf(
        coords = c("longitude", "latitude"),
        crs = 4326) |>
      st_transform(projected_crs) |>
      st_union() |>
      st_concave_hull(ratio = ratio) |>
      st_transform(4326) |>
      st_wrap_dateline(
        options = c(
          "WRAPDATELINE=YES",
          "DATELINEOFFSET=180"),
        quiet = TRUE) |>
      st_make_valid()
  }
  
  # run function and create a list of masks
  masks <- list(
    GOA = make_mask(goa_data, 3338),
    EBS = make_mask(ebs_data, 3338),
    California_Current = make_mask(cc_data, 5070),
    Northeast = make_mask(ne_data, 5070)
  )
  
  # plot masks to check them out
  
  # convert to sf object
  mask_sf <- purrr::imap_dfr(masks, \(geometry, region_name) {
      st_sf(
        region = region_name,
        geometry = st_geometry(geometry))
    }
  )
  
  # change lat/long from the standard −180° to 180° convention to a 0° to 360°
  mask_sf_shifted <- st_shift_longitude(mask_sf)
  
  # plot
  ggplot(mask_sf_shifted) +
    geom_sf(
      aes(fill = region),
      alpha = 0.4,
      color = "black") +
    coord_sf(
      xlim = c(160, 305),
      ylim = c(30, 67),
      expand = FALSE) +
    theme_bw()

