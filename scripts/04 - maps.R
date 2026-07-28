# maps of study area

  library(rnaturalearth)
  library(maps)
  library(here)
  library(tidyverse)
  library(sf)
  library(patchwork)

  #### set up ####
  
  # data for US West Coast stocks
  d <- readRDS(here("data for maps", "biol_haul_species_surveyjoin_12Nov2025.rds"))

  # get polygons

  # countries 
  countries <- ne_countries(returnclass = "sf")

  provinces <- ne_states(country = "canada", returnclass = "sf")

  region_countries <- countries |>
    filter(admin %in% c("United States of America", "Canada", "Mexico"))

  # jurisdiction lines = states/provinces
  states <- ne_states(country = c("United States of America"),
                      returnclass = "sf")

  # theme
  
  theme_map <- function(base_size = 11, base_family = "") {
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
      theme(
        axis.line        = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.title       = element_blank(),
        panel.background = element_blank(),
        panel.border     = element_blank(),
        panel.grid       = element_blank(),
        plot.background  = element_blank(),
        legend.background = element_blank(),
        legend.key       = element_blank()
      )
  }
  
  #### California Current ####
  
  # data
  WC <- d |>
    filter(str_detect(survey_id, "NWFSC"))
  
  # grid
  pts <- WC |>
    rename(latitude = lat_start,
           longitude = lon_start) |>
    select(latitude, longitude) |>
    drop_na()
  
  # convert to sf object
  pts_sf <- st_as_sf(pts, coords = c("longitude", "latitude"), crs = 4326)
  
  # unify into polygon
  poly_cc <- pts_sf |>
    summarise() |>
    st_convex_hull()
  
  # map
  cc_states <- states |>
    filter(
      name_en %in% c(
        "California", "Oregon", "Washington", "Nevada", "Idaho"))
  
  # map
  cc <-
    ggplot() +
    geom_sf(data = poly_cc, fill = "#1D8FFF", alpha = 0.5) +
    geom_sf(data = region_countries, fill = "lightgrey", color = "black", linewidth = 0.3) +
    geom_sf(data = cc_states, fill = NA, color = "black", linewidth = 0.2) +
    annotate("text", x = -129, y = 39.5,
      label = "California\nCurrent",
      color = "#1D8FFF",
      angle = 0,
      size = 4,
      fontface = "bold") +
    coord_sf(
      xlim = c(-140, -116),
      ylim = c(28, 51),
      expand = FALSE,
      crs = st_crs(4326)) +
    labs(x = "Longitude", y = "Latitude") +
    theme_map()
  
  # ggsave(cc, file = "./output/cc_map.tiff")
  
  #### AK ####
  
  AK <- d |>
    filter(str_detect(survey_id, "BS|AI|GOA"))
  
  pts_ebs <- d |>
    filter(str_detect(survey_id, "BS|AI")) |>
    rename(latitude = lat_start, longitude = lon_start) |>
    select(latitude, longitude) |>
    drop_na() |>
    filter(!(longitude > -158 & latitude < 55))
  
  pts_sf_ebs <- st_as_sf(pts_ebs, coords = c("longitude", "latitude"), crs = 4326)
  
  poly_ebs <- pts_sf_ebs |> summarise() |> st_concave_hull(ratio = 0.3)
  
  # GOA grid
  pts_goa <- d |>
    filter(str_detect(survey_id, "GOA")) |>
    rename(latitude = lat_start, longitude = lon_start) |>
    select(latitude, longitude) |>
    drop_na()
  
  pts_sf_goa <- st_as_sf(pts_goa, coords = c("longitude", "latitude"), crs = 4326)
  
  poly_goa <- pts_sf_goa |> summarise() |> st_concave_hull(ratio = 0.3)
  
  # polygons
  
  # map
  ak_states <- states |>
    filter(
      name_en %in% c("Alaska"))
  
  # map
  ak <- 
    ggplot() +
    geom_sf(data = poly_ebs, fill = "#E59F00", alpha = 0.5) +
    geom_sf(data = poly_goa,fill = "#009D73", alpha = 0.5) +
    geom_sf(data = region_countries, fill = "lightgrey", color = "black", linewidth = 0.3) +
    geom_sf(data = ak_states, fill = NA, color = "black", linewidth = 0.2) +
    annotate("text", x = -175, y = 65,
             label = "Bering\nSea",
             color = "#E59F00",
             angle = 0,
             size = 4,
             fontface = "bold") +
    annotate("text", x = -145, y = 56,
             label = "Gulf of\nAlaska",
             color = "#009D73",
             angle = 0,
             size = 4,
             fontface = "bold") +
    coord_sf(
      xlim = c(-180, -140),
      ylim = c(50, 75),
      expand = FALSE,
      crs = st_crs(4326)) +
    labs(x = "Longitude", y = "Latitude") +
    theme_map()
  
  #ggsave(ak, file = "./output/ak_map.tiff")
  
  #### plot together ####
  map <-  cc + plot_spacer() + ak +
    plot_layout(widths = c(1, 0.0005, 1))
  
  ggsave(
    here("output", "combined_maps.png"),
    plot = map,
    width = 8,
    height = 4,
    dpi = 300
  )
  
  
