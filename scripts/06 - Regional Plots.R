# DFA trends + loadings figure (models already fit)
library(tidyverse)
library(bayesdfa)
library(patchwork)

# region, trend inversion (so loadings fit positive), and map color
regions <- tibble::tribble(
  ~region,               ~invert, ~color,
  "California Current",   TRUE,    "#1D8FFF",
  "Gulf of Alaska",       FALSE,   "#009D73",
  "Bering Sea",           FALSE,    "#E59F00"
)
region_order <- regions$region

#### Shared scales ####
# symmetric, centered loading axis common to all regions
load_lim <- regions |>
  pmap_dbl(\(region, invert, color) {
    r <- rotate_trends(readRDS(paste0("./output/", region, ".rds")), invert = invert)
    max(abs(dfa_loadings(r, summary = FALSE)$loading))
  }) |>
  max() * c(-1.02, 1.02)

trend_ylim <- c(-5, 5)

clean <- theme(
  panel.grid       = element_blank(),
  strip.text       = element_blank(),
  strip.background = element_blank(),
  plot.margin      = margin(10, 15, 10, 15)
)

#### Panels ####
make_region_plots <- function(region, invert, color) {
  
  fit    <- readRDS(paste0("./output/", region, ".rds"))
  ts_key <- readRDS(paste0("./output/", region, "_ts_key.rds"))
  r      <- rotate_trends(fit, invert = invert)
  
  yrs       <- seq(min(biomass_dat$year[biomass_dat$subregion == region]),
                   max(biomass_dat$year[biomass_dat$subregion == region]))
  spp_names <- ts_key$ts[order(ts_key$ts_id)]
  
  trends <- plot_trends(r, years = yrs) +
    theme_bw() + clean +
    coord_cartesian(ylim = trend_ylim) +
    ylab(NULL)
  trends$layers[[1]]$aes_params$fill   <- color
  trends$layers[[2]]$aes_params$colour <- color
  
  # order species most-negative (top) to most-positive (bottom)
  ord <- dfa_loadings(r, summary = TRUE, names = spp_names)
  ord <- ord$name[order(ord$median)]
  
  loadings <- plot_loadings(r, names = spp_names) +
    theme_bw() + clean +
    xlab(NULL) +
    scale_fill_manual(values = color, guide = "none") +
    scale_alpha_continuous(
      name   = paste0(region, "\nP(loading \u2260 0)"),
      limits = c(0.5, 1), breaks = c(0.5, 0.75, 1), range = c(0.2, 1),
      guide  = guide_legend(override.aes = list(fill = color))
    ) +
    theme(
      legend.position    = c(0.98, 0.98),
      legend.justification = c(1, 1),
      legend.background  = element_rect(fill = alpha("white", 0.7), colour = NA),
      legend.key         = element_blank(),
      legend.title       = element_text(hjust = 0.5, face = "bold", size = 9),
      legend.text        = element_text(size = 8),
      legend.key.size    = unit(0.8, "lines")
    )
  loadings$data$name <- factor(loadings$data$name, levels = rev(ord))
  loadings <- loadings + coord_flip(ylim = load_lim)
  
  list(trends = trends, loadings = loadings)
}

drop_x <- function(p) p + theme(
  axis.title.x = element_blank(),
  axis.text.x  = element_blank(),
  axis.ticks.x = element_blank()
)

p <- pmap(regions, make_region_plots)
names(p) <- region_order

# y-axis title only on center-most trends panel
p[[2]]$trends <- p[[2]]$trends + ylab("Anomaly")

# a = trends column, b = loadings column (tag the top-row panels only)
p[[1]]$trends   <- p[[1]]$trends   + labs(tag = "a")
p[[1]]$loadings <- p[[1]]$loadings + labs(tag = "b")

#### Assemble ####
figure <- (
  (drop_x(p[[1]]$trends) | drop_x(p[[1]]$loadings)) /
    (drop_x(p[[2]]$trends) | drop_x(p[[2]]$loadings)) /
    (p[[3]]$trends        | p[[3]]$loadings)
) &
  theme(
    plot.tag = element_text(face = "bold", size = 14)
  )

ggsave("./output/DFA_all_regions.png", figure,
       width = 11, height = 12, dpi = 300, limitsize = FALSE)

#### Individual regional plots ####
walk(region_order, \(reg) {
  pr <- p[[reg]]
  regional <- (pr$trends + ylab("Anomaly") + labs(tag = NULL)) |
    (pr$loadings + labs(tag = NULL))
  ggsave(paste0("./output/DFA_", reg, ".png"), regional,
         width = 11, height = 4, dpi = 300, limitsize = FALSE)
})
