# DFA supporting figures: summary tables
library(tidyverse)
library(kableExtra)
library(here)

# region order
region_order <- c("California Current", "Gulf of Alaska", "Bering Sea")

#### Tables ####

# species used per region: stocks with >= 10 years (matches the fitted models)
region_species <- map_dfr(region_order, \(rg) {
  biomass_dat |>
    filter(subregion == rg) |>
    add_count(common_name) |>
    filter(n >= 10) |>
    distinct(common_name) |>
    mutate(region = rg)
})

codes <- c("California Current" = "CC", "Gulf of Alaska" = "GoA", "Bering Sea" = "BS")

# Table 1: region and number of stocks
tbl_counts <- region_species |>
  count(region, name = "Number of Stocks") |>
  mutate(region = factor(region, region_order)) |>
  arrange(region) |>
  rename(Region = region)

kbl_counts <- tbl_counts |>
  kbl(align = c("l", "c"), booktabs = TRUE) |>
  kable_styling(full_width = FALSE) |>
  row_spec(0, extra_css = "border-top: 2px solid black; border-bottom: 2px solid black;") |>
  row_spec(seq_len(nrow(tbl_counts)),
           extra_css = "border-bottom: 1px solid grey;") |>
  row_spec(nrow(tbl_counts),
           extra_css = "border-bottom: 2px solid black;")

save_kable(kbl_counts, "./output/table_stock_counts.html")

# Table 2: species x region, sorted by presence pattern
#   all three -> CC -> CC+GoA -> GoA -> GoA+BS -> BS -> BS+CC
pattern_rank <- c("CC,GoA,BS" = 1, "CC" = 2, "CC,GoA" = 3, "GoA" = 4,
                  "GoA,BS" = 5, "BS" = 6, "CC,BS" = 7)

# ---- Derive Linf source at the table ----
# CSV Linf keyed on sci_name; species_growth carries species + common_name + TLinfinity.
# Rule: CSV Linf present            -> "Stock assessment"
#       else TLinfinity present     -> "FishBase"
#       else (no Linf at all, crabs)-> "N/A"
csv_linf <- readr::read_csv(here("fish_names.csv")) |>
  select(sci_name, Linf)

linf_source_lookup <- species_growth |>
  select(common_name, species, TLinfinity) |>
  left_join(csv_linf, by = c("species" = "sci_name")) |>
  mutate(
    `Linf source` = case_when(
      !is.na(Linf)       ~ "Stock assessment",
      !is.na(TLinfinity) ~ "FishBase",
      TRUE               ~ "N/A"
    )
  ) |>
  select(common_name, `Linf source`) |>
  distinct()

tbl_species <- region_species |>
  mutate(present = "\u2022") |>
  pivot_wider(names_from = region, values_from = present, values_fill = "") |>
  rowwise() |>
  mutate(pattern = paste(codes[region_order[c_across(all_of(region_order)) == "\u2022"]],
                         collapse = ",")) |>
  ungroup() |>
  mutate(rank = pattern_rank[pattern]) |>
  arrange(rank, common_name) |>
  left_join(linf_source_lookup, by = "common_name") |>
  mutate(`Linf source` = replace_na(`Linf source`, "N/A")) |>
  select(Species = common_name, all_of(region_order), `Linf source`)

kbl_species <- tbl_species |>
  kbl(align = c("l", rep("c", length(region_order)), "c"), booktabs = TRUE) |>
  kable_styling(full_width = FALSE) |>
  row_spec(0, extra_css = "border-top: 2px solid black; border-bottom: 2px solid black;") |>
  row_spec(seq_len(nrow(tbl_species)),
           extra_css = "border-bottom: 1px solid grey;") |>
  row_spec(nrow(tbl_species),
           extra_css = "border-bottom: 2px solid black;") |>
  column_spec(1, border_left = TRUE) |>
  column_spec(ncol(tbl_species), border_right = TRUE)

save_kable(kbl_species, "./output/table_species_regions.html")