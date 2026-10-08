# DFA trends + loadings figure (models already fit)
  
  library(tidyverse)
  library(bayesdfa)
  library(patchwork)
  
  # region, trend inversion (so more loadings fit positive), and map color
  regions <- tribble(
    ~region,               ~invert, ~color,
    "Bering Sea",           FALSE,   "#E59F00",
    "Gulf of Alaska",       TRUE,    "#009D73",
    "Canada",               TRUE,    "#CC79A7",
    "California Current",   FALSE,   "#1D8FFF"
  )
  
  # shared trend axis
  trend_ylim <- c(-5, 5)
  
  # dark theme
  bg_col     <- "black"
  text_col   <- "white"
  axis_col   <- "grey85"
  border_col <- "grey50"
  ref_col    <- "grey60"   # zero / reference lines
  
  # shared dark theme
  dark_theme <- theme_bw() + theme(
    plot.background  = element_rect(fill = bg_col, colour = NA),
    panel.background = element_rect(fill = bg_col, colour = NA),
    panel.border     = element_rect(fill = NA, colour = border_col),
    panel.grid       = element_blank(),
    text             = element_text(colour = text_col),
    axis.text        = element_text(colour = axis_col),
    axis.title       = element_text(colour = text_col),
    axis.ticks       = element_line(colour = border_col),
    strip.text       = element_blank(),
    strip.background = element_blank(),
    plot.margin      = margin(10, 15, 10, 15)
  )
  
  # remove x axis from stacked panels
  no_x_axis <- theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )
  
  # load model and species key, rotate trend
  region_dat <- regions |>
    mutate(
      fit       = map(region, \(reg) readRDS(paste0("./output/", reg, ".rds"))),
      ts_key    = map(region, \(reg) readRDS(paste0("./output/", reg, "_ts_key.rds"))),
      rotated   = map2(fit, invert, \(f, inv) rotate_trends(f, invert = inv)),
      years     = map(rotated, \(r) seq(1993, length.out = ncol(r$trends_mean))),
      spp_names = map(ts_key, \(k) k |> arrange(ts_id) |> pull(ts)),
      max_load  = map_dbl(rotated, \(r) {
        dfa_loadings(r, summary = FALSE) |> pull(loading) |> abs() |> max()
      }),
      # order species most negative (top) to most positive (bottom)
      spp_order = map2(rotated, spp_names, \(r, nm) {
        dfa_loadings(r, summary = TRUE, names = nm) |> arrange(median) |> pull(name)
      })
    )
  
  # symmetric, centered loading axis common to all regions
  load_lim <- max(region_dat$max_load) * c(-1.02, 1.02)
  
  # build panels for every region
  region_dat <- region_dat |>
    mutate(
      # trends panel
      trends = pmap(list(rotated, years, color), \(r, yrs, col) {
        p <- plot_trends(r, years = yrs) +
          dark_theme +
          coord_cartesian(ylim = trend_ylim) +
          ylab(NULL)
        p$layers[[1]]$aes_params$fill   <- col
        p$layers[[2]]$aes_params$colour <- col
        p
      }),
      # loadings panel
      loadings = pmap(list(rotated, spp_names, spp_order, color, region), \(r, nm, ord, col, reg) {
        p <- plot_loadings(r, names = nm) +
          dark_theme +
          xlab(NULL) +
          scale_fill_manual(values = col, guide = "none") +
          scale_alpha_continuous(
            name   = paste0(reg, "\nP(loading ≠ 0)"),
            limits = c(0.5, 1), breaks = c(0.5, 0.75, 1), range = c(0.2, 1),
            guide  = guide_legend(override.aes = list(fill = col))
          ) +
          theme(
            legend.position      = c(0.98, 0.98),
            legend.justification = c(1, 1),
            legend.background    = element_rect(fill = alpha(bg_col, 0.7), colour = NA),
            legend.key           = element_blank(),
            legend.title         = element_text(hjust = 0.5, face = "bold", size = 9, colour = text_col),
            legend.text          = element_text(size = 8, colour = text_col),
            legend.key.size      = unit(0.8, "lines")
          ) +
          coord_flip(ylim = load_lim)
        p$data <- p$data |> mutate(name = factor(name, levels = rev(ord)))
        p
      })
    )
  
  # bayesdfa draws reference lines in default black; make them visible on black
  c(region_dat$trends, region_dat$loadings) |>
    walk(\(p) {
      p$layers |>
        keep(\(l) inherits(l$geom, c("GeomHline", "GeomVline", "GeomAbline"))) |>
        walk(\(l) l$aes_params$colour <- ref_col)
    })
  
  n_regions <- nrow(region_dat)
  
  # a = trends column, b = loadings column (tag the top row only)
  # stack regions, x axis on bottom row only
  rows <- region_dat |>
    mutate(
      row_id   = row_number(),
      trends   = map2(trends,   row_id, \(p, i) if (i == 1) p + labs(tag = "a") else p),
      loadings = map2(loadings, row_id, \(p, i) if (i == 1) p + labs(tag = "b") else p),
      row      = pmap(list(trends, loadings, row_id), \(tr, ld, i) {
        if (i < n_regions) (tr + no_x_axis) | (ld + no_x_axis) else tr | ld
      })
    ) |>
    pull(row)
  
  # black background for the patchwork canvas
  dark_canvas <- plot_annotation(
    theme = theme(plot.background = element_rect(fill = bg_col, colour = NA))
  )
  
  # shared "Anomaly" y title
  y_title <- wrap_elements(
    full = grid::textGrob("Biomass Anomaly", rot = 90,
                          gp = grid::gpar(col = text_col, fontsize = 11))
  ) + theme(plot.background = element_rect(fill = bg_col, colour = NA))
  
  stack <- wrap_plots(rows, ncol = 1)
  
  figure <- (y_title | stack) +
    plot_layout(widths = c(0.025, 1)) +
    dark_canvas &
    theme(plot.tag = element_text(face = "bold", size = 14, colour = text_col))
  
  # save combined figure
  ggsave("./output/DFA_all_regions_dark.png", figure,
         width = 11, height = 4 * n_regions, dpi = 300, limitsize = FALSE, bg = bg_col)
  
  # save individual regional plots
  region_dat |>
    select(region, trends, loadings) |>
    pwalk(\(region, trends, loadings) {
      regional <- ((trends + ylab("Anomaly")) | loadings) + dark_canvas
      ggsave(paste0("./output/DFA_", region, "_dark.png"), regional,
             width = 11, height = 4, dpi = 300, limitsize = FALSE, bg = bg_col)
    })