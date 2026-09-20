#04 - Import fish type and size through fishbase

  library(rfishbase)
  library(tidyverse)
  library(here)
  
  # fishbase species codes
  fish_names <- c(
    "Reinhardtius hippoglossoides", "Pleurogrammus monopterygius", "Bathyraja parmifera",
    "Limanda aspera", "Pleuronectes quadrituberculatus", "Atheresthes evermanni",
    "Anoplopoma fimbria", "Atheresthes stomias", "Hippoglossoides elassodon",
    "Sebastes alutus", "Lepidopsetta polyxystra", "Sebastes polyspinis",
    "Gadus macrocephalus", "Sebastolobus altivelis", "Parophrys vetulus",
    "Ophiodon elongatus", "Sebastes entomelas", "Sebastes aurora",
    "Sebastes melanops", "Scorpaenichthys marmoratus",
    "Hexagrammos decagrammus", "Scorpaena guttata", "Scomber japonicus",
    "Merluccius productus", "Sebastes jordani", "Sardinops sagax",
    "Microstomus pacificus", "Glyptocephalus zachirus", "Sebastes ciliatus",
    "Lepidopsetta bilineata", "Beringraja rhina", "Gadus chalcogrammus"
  )
  
  sp_table <- fb_tbl("species")
  
  sp_table$species <- paste0(sp_table$Genus, " ", sp_table$Species)
  
  sp_table <- sp_table |>
    select(SpecCode, DemersPelag, species)
  
  # trim to species of interest
  sp_table_trim <- sp_table |>
    filter(species %in% fish_names)
  
  species_codes <- sp_table_trim$SpecCode
  
  # get mass
  
  growth_tb <- growth_tb |>
    select(SpecCode, TLinfinity, Locality)
  
  # filter by locality
  growth_sel <- growth_tb |>
    filter(!is.na(TLinfinity)) |>
    filter(
      (SpecCode == 117 & Locality == "California") |
        (SpecCode == 308 & Locality == "Bering Sea (Virgin stock)") |
        (SpecCode == 308 & Locality == "Hecate Strait") |
        (SpecCode == 308 & Locality == "Eastern Bering Sea") |
        (SpecCode == 308 & Locality == "Bering Sea") |
        (SpecCode == 326 & Locality == "off Portland, Oregon , 100 m depth") |
        (SpecCode == 326 & Locality == "Northeast Pacific") |
        (SpecCode == 504 & Locality == "Bering Sea") |
        (SpecCode == 504 & Locality == "`Gulf of Alaska'") |
        (SpecCode == 504 & Locality == "off Vancouver Island") |
        (SpecCode == 504 & Locality == "Northeast Pacific") |
        (SpecCode == 504 & Locality == "British Columbia") |
        (SpecCode == 504 & Locality == "Gulf of Alaska") |
        (SpecCode == 504 & Locality == "Aleutian Islands") |
        (SpecCode == 509 & Locality == "Strait of Georgia") |
        (SpecCode == 509 & Locality == "Vancouver I. (west coast)") |
        (SpecCode == 512 & Locality == "Oregon") |
        (SpecCode == 512 & Locality == "Northeast Pacific") |
        (SpecCode == 512 & Locality == "West coast") |
        (SpecCode == 512 & Locality == "Gulf of Alaska") |
        (SpecCode == 512 & Locality == "Vancouver I. (west coast)") |
        (SpecCode == 512 & Locality == "Queen Charlotte Sound (east coast inlets)") |
        (SpecCode == 512 & Locality == "Queen Charlotte Is. (west coast)") |
        (SpecCode == 512 & Locality == "West coast of USA and Canada") |
        (SpecCode == 512 & Locality == "Bering Sea") |
        (SpecCode == 512 & Locality == "Aleutian Islands") |
        (SpecCode == 516 & Locality == "Bering Sea") |
        (SpecCode == 517 & Locality == "Gulf of Alaska") |
        (SpecCode == 518 & Locality == "Bering Sea") |
        (SpecCode == 519 & Locality == "Gulf of Alaska") |
        (SpecCode == 520 & Locality == "East bering Sea") |
        (SpecCode == 520 & Locality == "Southeastern Bering Sea") |
        (SpecCode == 520 & Locality == "Aleutian Islands") |
        (SpecCode == 1477 & Locality == "California") |
        (SpecCode == 1477 & Locality == "British Columbia") |
        (SpecCode == 1477 & Locality == "San Francisco, California") |
        (SpecCode == 1477 & Locality == "Monterey, California") |
        (SpecCode == 1477 & Locality == "Guaymas, Gulf of California") |
        (SpecCode == 1477 & Locality == "Gulf of California") |
        (SpecCode == 1477 & Locality == "Bahia Magdalena, Baja California") |
        (SpecCode == 3943 & Locality == "Santa Monica, California, 30 m") |
        (SpecCode == 3943 & Locality == "South California Bight") |
        (SpecCode == 3974 & Locality == "Northern California-Central Oregon coast") |
        (SpecCode == 3974 & Locality == "California") |
        (SpecCode == 3979 & Locality == "California") |
        (SpecCode == 3979 & Locality == "between Monterey Bay and Morro Bay") |
        (SpecCode == 3979 & Locality == "off Oregon") |
        (SpecCode == 3990 & Locality == "Northeast Pacific") |
        (SpecCode == 4010 & Locality == "Northeast Pacific") |
        (SpecCode == 4037 & Locality == "Gulf of Alaska") |
        (SpecCode == 4140 & Locality == "California") |
        (SpecCode == 4247 & Locality == "California") |
        (SpecCode == 4247 & Locality == "central California") |
        (SpecCode == 4248 & Locality == "Puget Sound, Washington") |
        (SpecCode == 4250 & Locality == "Bering Sea (Virgin Stock)") |
        (SpecCode == 4250 & Locality == "Bering Sea") |
        (SpecCode == 24237 & Locality == "Bering Sea (Virgin Stock)") |
        (SpecCode == 24237 & Locality == "Northeast Pacific") |
        (SpecCode == 24237 & Locality == "Western coast") |
        (SpecCode == 24237 & Locality == "Kodiak Island") |
        (SpecCode == 58882 & Locality == "Kodiak Island") 
    )
  
  # average TLinfinity to one value per species
  growth_avg <- growth_sel |>
    group_by(SpecCode) |>
    summarise(TLinfinity = mean(TLinfinity, na.rm = TRUE), .groups = "drop")
  
  # rejoin full FishBase list so no species is dropped
  fish_full <- sp_table_trim |>
    left_join(growth_avg, by = "SpecCode")
  
  # add non-FishBase species: crabs (benthic)
  extra_spp <- tibble(
    SpecCode    = NA_integer_,
    DemersPelag = rep("bathydemersal", 4),
    species     = c("Paralithodes platypus", "Paralithodes camtschaticus",
                    "Chionoecetes bairdi",   "Chionoecetes opilio"),
    TLinfinity  = NA_real_
  )
  
  # ---- Combine ----
  species_growth <- bind_rows(fish_full, extra_spp)
  
  # ---- Attach common_name ----
  name_lookup <- readr::read_csv(here("fish_names.csv"))
  species_growth <- species_growth |>
    left_join(name_lookup, by = c("species" = "sci_name")) |>
    # fill missing fishbase values from csv linf
    mutate(TLinfinity = coalesce(TLinfinity, Linf)) |>
    select(-Linf) |>
    as.data.frame()
  
  # ---- Checks ----
  nrow(species_growth)                                   # expect 37
  sum(is.na(species_growth$TLinfinity))                  # remaining NAs after csv fill
  species_growth |> filter(is.na(common_name)) |> select(SpecCode, species)  # expect 0 rows