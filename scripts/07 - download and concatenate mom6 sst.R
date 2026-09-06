  # 07 - download and concatenate mom6 hindcast temperature

  library(tidyverse)
  library(ncdf4)
  library(sf)
  library(lubridate)
  library(here)

  # define NEP url
  nep_tos_url <- paste0(
    "https://psl.noaa.gov/thredds/dodsC/",
    "Projects/CEFI/regional_mom6/cefi_portal/",
    "northeast_pacific/full_domain/hindcast/monthly/regrid/",
    "r20260701/",
    "tos.nep.full.hcast.monthly.regrid.",
    "r20260701.199301-202512.nc")
  
  # define NWA url
  nwa_tos_url <- paste0(
    "https://psl.noaa.gov/thredds/dodsC/",
    "Projects/CEFI/regional_mom6/cefi_portal/",
    "northwest_atlantic/full_domain/hindcast/monthly/regrid/",
    "r20250715/",
    "tos.nwa.full.hcast.monthly.regrid.",
    "r20250715.199301-202312.nc")
  
  # function to download mom6
  download_mom6 <- function(mask_polygon, url, variable = "tos") {
  
    nc <- nc_open(url)
    on.exit(nc_close(nc))
  
    # Read model coordinates
    lon <- as.numeric(ncvar_get(nc, "lon"))
    lat <- as.numeric(ncvar_get(nc, "lat"))
  
    # mom6 NEP uses 0–360° longitude so need to shift the polygon 
    # to match the model longitude convention
    
    if (min(lon) >= 0 && max(lon) > 180) {
      mask_model <- st_shift_longitude(mask_polygon)
      lon_model <- lon
    } else {
      mask_model <- mask_polygon
      lon_model <- lon
    }
  
    # read and convert time
    time_values <- ncvar_get(nc, "time")
  
    time_units <- ncatt_get(
      nc, "time", "units")$value
  
    origin <- str_extract(
      time_units,
      "(?<=since\\s)\\d{4}-\\d{2}-\\d{2}")
  
    dates <- as.Date(
      time_values,
      origin = origin)
  
    # find bounding rectangle around mask
    mask_bbox <- st_bbox(mask_model)
  
    lon_indices <- which(
      lon_model >= mask_bbox["xmin"] &
        lon_model <= mask_bbox["xmax"])
  
    lat_indices <- which(
      lat >= mask_bbox["ymin"] &
        lat <= mask_bbox["ymax"])
  
    if (length(lon_indices) == 0 || length(lat_indices) == 0) {
      stop("The mask does not overlap the MOM6 domain.")
    }
  
    lon_indices <- seq(
      min(lon_indices),
      max(lon_indices))
  
    lat_indices <- seq(
      min(lat_indices),
      max(lat_indices))
  
    # Create coordinate grid for the downloaded rectangle
    download_grid <- expand.grid(
      lon_index = lon_indices,
      lat_index = lat_indices) |>
      as_tibble() |>
      mutate(
        lon_model = lon_model[lon_index],
        lat = lat[lat_index])
  
    download_grid_sf <- download_grid |>
      st_as_sf(
        coords = c("lon_model", "lat"),
        crs = 4326,
        remove = FALSE)
  
    # Identify model cells inside the mask
    download_grid <- download_grid |>
      mutate(
        inside_mask = lengths(
          st_intersects(
            download_grid_sf,
            mask_model)) > 0)
  
    message(
      sum(download_grid$inside_mask),
      " model cells inside mask")
  
    # download each month
    output <- map_dfr(seq_along(dates), function(m) {
  
      message(
        "Processing: ",
        format(dates[m], "%Y-%m"))
  
      model_slice <- ncvar_get(
        nc,
        variable,
        start = c(
          min(lon_indices),
          min(lat_indices),
          m),
        count = c(
          length(lon_indices),
          length(lat_indices),
          1)) |>
        drop()
  
      download_grid |>
        mutate(
          # Convert output back to −180–180°
          lon = if_else(
            lon_model > 180,
            lon_model - 360,
            lon_model),
          value = as.vector(model_slice),
          date = dates[m]) |>
        filter(inside_mask) |>
        select(lon, lat, date, value)
    })
  
    names(output)[names(output) == "value"] <- variable
  
    output
  }
  
  # function to summarize temp annually
  summarize_annual <- function(data) {
  
    data |>
      filter(!is.na(tos)) |>
      mutate(year = year(date)) |>
      group_by(year) |>
      summarise(
        mean_tos = mean(tos),
        .groups = "drop")
  }
  
  
  # GOA
  temp_goa <- download_mom6(
    mask_polygon = masks$GOA,
    url = nep_tos_url)
  
  temp_goa_annual <- summarize_annual(temp_goa)
  
  saveRDS(
    temp_goa_annual,
    here("temp output", "mom6_goa_annual_sst.rds"))
  
  
  # EBS
  temp_ebs <- download_mom6(
    mask_polygon = masks$EBS,
    url = nep_tos_url)
  
  temp_ebs_annual <- summarize_annual(temp_ebs)
  
  saveRDS(
    temp_ebs_annual,
    here("temp output", "mom6_ebs_annual_sst.rds"))
  
  
  # California Current
  temp_cc <- download_mom6(
    mask_polygon = masks$California_Current,
    url = nep_tos_url)
  
  temp_cc_annual <- summarize_annual(temp_cc)
  
  saveRDS(
    temp_cc_annual,
    here("temp output", "mom6_cc_annual_sst.rds"))
  
  
  # Northeast
  temp_ne <- download_mom6(
    mask_polygon = masks$Northeast,
    url = nwa_tos_url)
  
  temp_ne_annual <- summarize_annual(temp_ne)
  
  saveRDS(
    temp_ne_annual,
    here("temp output", "mom6_ne_annual_sst.rds"))