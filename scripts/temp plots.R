# check temo output with maps

library(tidyverse)
library(sf)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)

# Get global land polygons
land <- ne_countries(
  scale = "medium",
  returnclass = "sf"
) |>
  st_make_valid() |>
  st_shift_longitude()

# Land polygons in the standard -180 to 180 convention
land <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) |>
  st_make_valid()

map_annual_temp <- function(
    temp_data,
    mask_polygon,
    region_name,
    ncol = 5) {

  # Does this region cross the international dateline?
  crosses_dateline <- diff(
    range(temp_data$lon, na.rm = TRUE)
  ) > 180

  # Use 0-360 only for dateline-crossing regions such as EBS
  if (crosses_dateline) {

    temp_data <- temp_data |>
      mutate(
        lon_plot = if_else(
          lon < 0,
          lon + 360,
          lon
        )
      )

    mask_plot <- st_shift_longitude(mask_polygon)
    land_plot <- st_shift_longitude(land)

  } else {

    temp_data <- temp_data |>
      mutate(lon_plot = lon)

    mask_plot <- mask_polygon
    land_plot <- land
  }

  # Annual mean at each spatial grid cell
  annual_grid <- temp_data |>
    mutate(year = year(date)) |>
    group_by(lon_plot, lat, year) |>
    summarise(
      mean_tos = mean(tos, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(is.finite(mean_tos))

  # Get limits from temperature data, not the polygon
  x_limits <- range(
    annual_grid$lon_plot,
    na.rm = TRUE
  ) + c(-0.5, 0.5)

  y_limits <- range(
    annual_grid$lat,
    na.rm = TRUE
  ) + c(-0.5, 0.5)

  # Crop land to the plotting region
  land_local <- suppressWarnings(
    st_crop(
      land_plot,
      xmin = x_limits[1],
      xmax = x_limits[2],
      ymin = y_limits[1],
      ymax = y_limits[2]
    )
  )

  ggplot() +

    # Draw land underneath the SST
    geom_sf(
      data = land_local,
      fill = "grey85",
      color = "grey45",
      linewidth = 0.15
    ) +

    # Draw SST over the land layer
    geom_tile(
      data = annual_grid,
      aes(
        x = lon_plot,
        y = lat,
        fill = mean_tos
      )
    ) +

    # Survey-footprint outline
    geom_sf(
      data = mask_plot,
      fill = NA,
      color = "black",
      linewidth = 0.35
    ) +

    # Region is in title; year is in each facet
    facet_wrap(
      ~ year,
      ncol = ncol
    ) +

    coord_sf(
      xlim = x_limits,
      ylim = y_limits,
      expand = FALSE,
      default_crs = st_crs(4326)
    ) +

    scale_fill_viridis_c(
      option = "plasma",
      name = "Annual mean\nSST (°C)"
    ) +

    labs(
      title = region_name,
      x = "Longitude",
      y = "Latitude"
    ) +

    theme_bw() +

    theme(
      plot.title = element_text(
        face = "bold",
        hjust = 0.5
      ),
      strip.background = element_rect(
        fill = "white",
        color = "grey50"
      ),
      strip.text = element_text(size = 9),
      panel.grid = element_blank(),
      legend.position = "bottom"
    )
}

p_goa <- map_annual_temp(
  temp_goa,
  masks$GOA,
  "Gulf of Alaska"
)

p_ebs <- map_annual_temp(
  temp_ebs,
  masks$EBS,
  "Eastern Bering Sea"
)

p_cc <- map_annual_temp(
  temp_cc,
  masks$California_Current,
  "California Current"
)

p_ne <- map_annual_temp(
  temp_ne,
  masks$Northeast,
  "Northeast U.S."
)

# Save Gulf of Alaska map
ggsave(
  filename = here(
    "temp output",
    "mom6_sst_maps_goa.png"
  ),
  plot = p_goa,
  width = 14,
  height = 14,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Save Eastern Bering Sea map
ggsave(
  filename = here(
    "temp output",
    "mom6_sst_maps_ebs.png"
  ),
  plot = p_ebs,
  width = 14,
  height = 14,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Save California Current map
ggsave(
  filename = here(
    "temp output",
    "mom6_sst_maps_cc.png"
  ),
  plot = p_cc,
  width = 14,
  height = 14,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Save Northeast U.S. map
ggsave(
  filename = here(
    "temp output",
    "mom6_sst_maps_ne.png"
  ),
  plot = p_ne,
  width = 14,
  height = 14,
  units = "in",
  dpi = 300,
  bg = "white"
)

## annual time series
# Combine annual SST summaries
temp_annual_all <- bind_rows(
  "Gulf of Alaska" = temp_goa_annual,
  "Eastern Bering Sea" = temp_ebs_annual,
  "California Current" = temp_cc_annual,
  "Northeast U.S." = temp_ne_annual,
  .id = "region"
)

# Plot annual SST time series
p_temp_annual <- ggplot(
  temp_annual_all,
  aes(
    x = year,
    y = mean_tos
  )
) +
  geom_line(
    linewidth = 0.8,
    color = "steelblue"
  ) +
  geom_point(
    size = 1.5,
    color = "steelblue"
  ) +
  facet_wrap(
    ~ region,
    scales = "free_y",
    ncol = 2
  ) +
  labs(
    x = "Year",
    y = "Annual mean SST (°C)"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(
      fill = "white",
      color = "grey50"
    ),
    strip.text = element_text(
      face = "bold"
    ),
    panel.grid.minor = element_blank()
  )

p_temp_annual