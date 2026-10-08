library(ggplot2)
library(sf)
library(rnaturalearth)

# Install once for detailed coastlines:
# pak::pkg_install("ropensci/rnaturalearthhires")

# Requires your existing mask_sf_shifted object.

# Detailed United States and Canada polygons
land_shifted <- ne_countries(
  country = c("United States of America", "Canada"),
  scale = 10,
  returnclass = "sf"
) |>
  st_shift_longitude()

# Names must match the values in mask_sf_shifted$region
region_colors <- c(
  "Eastern Bering Sea"  = "#E69F00",
  "Gulf of Alaska"      = "#009E73",
  "West Coast of Canada" = "#9932CC",
  "California Current" = "#1E90FF"
)

# Black background, white text, and a panel border
theme_map <- function(base_size = 11, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(color = "white"),
      axis.line = element_blank(),
      axis.text = element_text(size = 9, color = "white"),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_rect(fill = "black", color = NA),
      panel.border = element_rect(
        color = "white", fill = NA, linewidth = 0.5
      ),
      panel.grid.major = element_line(
        color = "grey30", linewidth = 0.2
      ),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "black", color = NA),
      legend.background = element_rect(fill = "black", color = NA),
      legend.key = element_rect(fill = "black", color = NA),
      legend.text = element_text(color = "white", size = 14),
      legend.title = element_blank()
    )
}

# Draw masks first, then land with thin coastline outlines
map_plot <- ggplot() +
  geom_sf(
    data = mask_sf_shifted,
    aes(fill = region, color = region),
    alpha = 0.7,
    linewidth = 0.3
  ) +
  geom_sf(
    data = land_shifted,
    fill = "grey80",
    color = "black",
    linewidth = 0.1
  ) +
  scale_fill_manual(
    values = region_colors,
    breaks = names(region_colors)
  ) +
  scale_color_manual(
    values = region_colors,
    guide = "none"
  ) +
  scale_x_continuous(breaks = seq(180, 240, by = 20)) +
  scale_y_continuous(breaks = seq(30, 60, by = 10)) +
  coord_sf(
    xlim = c(170, 245),
    ylim = c(30, 67),
    expand = FALSE
  ) +
  guides(
  fill = guide_legend(
    override.aes = list(alpha = 1, colour = NA)
  )) +
  theme_map()

map_plot

# Save at high resolution
ggsave(here(
  "output",
  "combined_maps.png"),
  plot = map_plot,
  device = ragg::agg_png,
  width = 8,
  height = 5,
  units = "in",
  dpi = 300,
  bg = "black"
)
