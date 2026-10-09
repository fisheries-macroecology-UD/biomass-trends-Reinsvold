# 05 - Linfinity vs DFA trend loading across subregions

  # load packages
  library(tidyverse)
  library(purrr)
  library(bayesdfa)
  library(broom)
  library(here)
  
  # species_growth (from script 04): common_name, TLinfinity
  regions <- c("Bering Sea", "Gulf of Alaska", "California Current")
  
  # pull loadings from each region's saved dfa fit
  get_loadings <- function(region) {
    fit    <- readRDS(paste0("./output/", region, ".rds"))
    ts_key <- readRDS(paste0("./output/", region, "_ts_key.rds"))
    
    # invert to match the convention used when plotting (majority positive)
    r <- rotate_trends(fit, invert = TRUE)
    
    # one-trend model: Z_rot_mean is [n_species x 1], ordered by ts_id
    loading <- as.numeric(r$Z_rot_mean[, 1])
    
    ts_key |>
      arrange(ts_id) |>
      transmute(common_name = ts, loading, subregion = region)
  }
  
  # map loadings across regions and flip gulf of alaska sign
  loadings_by_subregion <- map(regions, get_loadings) |>
    list_rbind() |>
    # gulf of alaska and Bering Sea: flip sign so majority of loadings are positive, matching other regions
    mutate(loading = if_else(subregion %in% c("Gulf of Alaska", "Bering Sea"), -loading, loading))
  
  # join linfinity by common_name, drop missing
  plot_dat <- loadings_by_subregion |>
    left_join(
      species_growth |> select(common_name, TLinfinity) |> distinct(),
      by = "common_name"
    ) |>
    filter(!is.na(TLinfinity), !is.na(loading))
  
  # per-subregion linear fit + slope + ci + pearson r (purrr)
  fit_stats <- plot_dat |>
    group_split(subregion) |>
    map_dfr(function(d) {
      sr <- unique(d$subregion)
      if (nrow(d) < 3) {
        return(tibble(subregion = sr, r = NA_real_,
                      slope = NA_real_, conf.low = NA_real_, conf.high = NA_real_,
                      label = paste0("n = ", nrow(d))))
      }
      m  <- lm(loading ~ TLinfinity, data = d)
      ci <- broom::tidy(m, conf.int = TRUE) |> filter(term == "TLinfinity")
      r <- cor(d$TLinfinity, d$loading, use = "complete.obs")
      tibble(
        subregion = sr,
        r         = r,
        slope     = ci$estimate,
        conf.low  = ci$conf.low,
        conf.high = ci$conf.high,
        label     = paste0("slope = ", signif(ci$estimate, 3),
                           "\n95% CI [", signif(ci$conf.low, 3), ", ", signif(ci$conf.high, 3), "]",
                           "\nr = ", round(r, 2), " (n = ", nrow(d), ")")
      )
    })
  
  # compute label positions for each subregion
  label_pos <- plot_dat |>
    group_by(subregion) |>
    summarise(
      x = min(TLinfinity, na.rm = TRUE) + 0.08 * diff(range(TLinfinity)),
      y = max(loading,    na.rm = TRUE) - 1.20 * diff(range(loading)),
      .groups = "drop"
    ) |>
    left_join(fit_stats, by = "subregion")
  
  # faceted plot
  p <- ggplot(plot_dat, aes(TLinfinity, loading)) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey70") +
    geom_point(alpha = 0.7, size = 2) +
    # se = TRUE draws the 95% ci ribbon around the lm fit
    geom_smooth(method = "lm", se = TRUE, colour = "steelblue",
                fill = "steelblue", alpha = 0.15,
                linewidth = 0.8, na.rm = TRUE) +
    geom_text(data = label_pos, aes(x, y, label = label),
              hjust = 0, vjust = 1, size = 3, inherit.aes = FALSE) +
    facet_wrap(~ subregion, scales = "free_x") +
    coord_cartesian(ylim = c(-1, 1)) +
    labs(
      x = expression("Maximum size " * (italic(L)[infinity]~"") * " (cm)"),
      y = "Anomaly"
    ) +
    theme_bw(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "grey95"),
      panel.grid = element_blank()
    )
  
  # save scatter plot
  ggsave(here("output", "linf_vs_loading_by_subregion.png"),
         p, width = 11, height = 5, dpi = 300)
  print(p)
  
  # box plots of trend loading by biological grouping, one panel per region
  # biological grouping = DemersPelag from script 04 (species_growth)
  
  # attach biological grouping to the loadings data, drop species with na grouping or loading
  box_dat <- loadings_by_subregion |>
    left_join(
      species_growth |> select(common_name, DemersPelag) |> distinct(),
      by = "common_name"
    ) |>
    filter(!is.na(loading), !is.na(DemersPelag)) |>
    mutate(
      subregion = factor(
        subregion,
        levels = c("California Current", "Gulf of Alaska", "Bering Sea")
      )
    )
  
  # build box plot
  p_box <- ggplot(box_dat, aes(x = DemersPelag, y = loading, fill = DemersPelag)) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey70") +
    geom_boxplot(alpha = 0.6, outlier.shape = NA) +
    geom_jitter(width = 0.15, height = 0, alpha = 0.6, size = 1.5) +
    facet_wrap(~ subregion, ncol = 1, scales = "free_x") +
    labs(
      x = "Biological grouping (DemersPelag)",
      y = "Anomaly"
    ) +
    theme_bw(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "grey95"),
      axis.text.x = element_text(angle = 30, hjust = 1),
      legend.position = "none",
      panel.grid = element_blank()
    )
  
  # save box plot
  ggsave(here("output", "loading_by_grouping_by_region.png"),
         p_box, width = 8, height = 11, dpi = 300)
  print(p_box)