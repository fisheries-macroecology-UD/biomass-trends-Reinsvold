# bottom temperature vs DFA trend by region
  
  library(tidyverse)
  library(bayesdfa)
  library(ggrepel)
  
  # region, trend inversion and color
  regions <- tribble(
    ~region,               ~invert, ~color,    ~bt_stub,
    "Canada",               TRUE,    "#CC79A7", "mom6_can",
    "Bering Sea",           FALSE,   "#E59F00", "mom6_ebs",
    "Gulf of Alaska",       TRUE,    "#009D73", "mom6_goa",
    "California Current",   FALSE,   "#1D8FFF", "mom6_cc"
  )
  region_cols <- set_names(regions$color, regions$region)
  
  # dark theme
  bg_col     <- "black"
  text_col   <- "white"
  axis_col   <- "grey85"
  border_col <- "grey50"
  ref_col    <- "grey60"
  
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
  
  # facet strips show region names here
  strip_theme <- theme(
    strip.background = element_rect(fill = "grey15", colour = border_col),
    strip.text       = element_text(colour = text_col)
  )
  
  # pair annual dfa trend (1993 onwards) with bottom temp in the same year
  plot_dat <- regions |>
    mutate(
      trend = map2(region, invert, \(reg, inv) {
        r <- readRDS(paste0("./output/", reg, ".rds")) |> rotate_trends(invert = inv)
        tibble(
          year    = seq(1993, length.out = ncol(r$trends_mean)),
          anomaly = r$trends_mean[1, ]
        )
      }),
      bt     = map(bt_stub, \(stub) readRDS(paste0("./temp output/", stub, "_annual_bt.rds"))),
      paired = map2(trend, bt, \(t, b) inner_join(t, b, by = "year"))
    ) |>
    select(region, paired) |>
    unnest(paired) |>
    mutate(region = factor(region, levels = regions$region))
  
  # scatter in region color with lm fit and non-overlapping year labels
  bt_layers <- list(
    geom_hline(yintercept = 0, linewidth = 0.3, colour = ref_col),
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                alpha = 0.2, linewidth = 0.8),
    geom_point(alpha = 0.9, size = 1.8),
    geom_text_repel(aes(label = year), size = 3, colour = "grey80",
                    segment.colour = "grey50", segment.size = 0.2,
                    max.overlaps = Inf, seed = 650),
    scale_colour_manual(values = region_cols, guide = "none"),
    scale_fill_manual(values = region_cols, guide = "none"),
    labs(
      x = expression("Bottom temperature " * (degree*C)),
      y = "Biomass Anomaly"
    ),
    dark_theme,
    strip_theme
  )
  
  # all regions in one row, free scales
  p_all <- plot_dat |>
    ggplot(aes(mean_tob, anomaly, colour = region, fill = region)) +
    bt_layers +
    facet_wrap(~ region, nrow = 1, scales = "free")
  
  ggsave("./output/bt_vs_anomaly_all_regions.png", p_all,
         width = 16, height = 4.5, dpi = 300, bg = bg_col)
  print(p_all)
  
  # one plot per region
  regions |>
    pull(region) |>
    walk(\(reg) {
      p <- plot_dat |>
        filter(region == reg) |>
        ggplot(aes(mean_tob, anomaly, colour = region, fill = region)) +
        bt_layers +
        facet_wrap(~ region)
      ggsave(paste0("./output/bt_vs_anomaly_", reg, ".png"), p,
             width = 5, height = 4.5, dpi = 300, bg = bg_col)
    })
  
  # linear model per region: anomaly ~ bottom temperature (same fit as the plotted lines)
  # significant if the 95% CI on the slope does not cross zero
  # (lower and upper bounds have the same sign, so their product is positive)
  lm_results <- plot_dat |>
    nest(data = -region) |>
    mutate(
      fit       = map(data, \(d) lm(anomaly ~ mean_tob, data = d)),
      n         = map_int(data, nrow),
      slope     = map_dbl(fit, \(f) coef(f)[["mean_tob"]]),
      ci_lower  = map_dbl(fit, \(f) confint(f, "mean_tob", level = 0.95)[1]),
      ci_upper  = map_dbl(fit, \(f) confint(f, "mean_tob", level = 0.95)[2]),
      p_value   = map_dbl(fit, \(f) summary(f)$coefficients["mean_tob", "Pr(>|t|)"]),
      r_squared = map_dbl(fit, \(f) summary(f)$r.squared),
      significant = ci_lower * ci_upper > 0
    ) |>
    select(-data, -fit) |>ss
    arrange(region)
  
  print(lm_results)
  write_csv(lm_results, "./output/bt_vs_anomaly_lm_results.csv")