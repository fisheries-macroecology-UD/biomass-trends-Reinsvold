  library(tidyverse)
  library(ncdf4)
  library(sf)
  library(lubridate)
  library(terra)
  library(here)
  
  # install.packages("pak")
  pak::pkg_install("DFO-NOAA-Pacific/surveyjoin")

  library(surveyjoin)
  
  theme_map <- function(base_size = 11, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      axis.line         = element_blank(),
      axis.text         = element_text(size = 9, color = "grey40"),
      axis.ticks        = element_blank(),
      axis.title        = element_blank(),
      panel.background  = element_rect(fill = "white", color = NA),
      panel.border      = element_blank(),
      panel.grid.major  = element_line(color = "grey90", linewidth = 0.2),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "white", color = NA),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.title      = element_blank()
    )
}
  
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
  
  # dfo pacific biological station grid
  can_data <- surveyjoin::dfo_synoptic_grid |>
    rename(longitude = lon,
           latitude = lat)
  
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
    Can = make_mask(can_data, 32610),
    California_Current = make_mask(cc_data, 5070) #,
   # Northeast = make_mask(ne_data, 5070)
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
  
  mask_sf_shifted <- mask_sf_shifted |>
  mutate(
    region = factor(
      region,
      levels = c(
        "EBS",
        "GOA",
        "Can",
     #   "Northeast",
        "California_Current"
      ),
      labels = c(
        "Eastern Bering Sea",
        "Gulf of Alaska",
        "West Coast of Canada",
      #  "U.S. Northeast Shelf",
        "California Current"
      )
    )
  )
  

# make new plots 
  
  # Install once
pak::pkg_install(c("rnaturalearth", "rnaturalearthdata"))

# United States and Canada polygons
land_shifted <- rnaturalearth::ne_countries(
  country = c("United States of America", "Canada"),
  scale = "medium",
  returnclass = "sf"
) |>
  st_shift_longitude()

p <- ggplot() +
  geom_sf(
    data = land_shifted,
    fill = "grey90",
    color = "grey40",
    linewidth = 0.3
  ) +
  geom_sf(
    data = mask_sf_shifted,
    aes(fill = region),
    alpha = 0.4,
    color = "black"
  ) +
  coord_sf(
  xlim = c(170, 245),
  ylim = c(30, 67),
  expand = FALSE
) +
  scale_x_continuous(breaks = seq(180, 240, by = 20)) +
  scale_y_continuous(breaks = seq(30, 60, by = 10)) + 
  labs(fill = "Region") +
  scale_fill_manual(
  values = c(
    "Eastern Bering Sea"   = "#E69F00",
    "Gulf of Alaska"       = "#009E73",
    "West Coast of Canada"     = "#9932CC",
   # "U.S. Northeast Shelf" = "#D55E00",
    "California Current"  = "#1E90FF"
  )) +
  theme_map() +
  theme(
    panel.border = element_rect(
    color = "grey40",
    fill = NA,
    linewidth = 0.5
  ))
  
   p
 
   
     ggsave(
    here("output", "combined_maps.png"),
    plot = p,
    width = 8,
    height = 4,
    dpi = 300
  )
  