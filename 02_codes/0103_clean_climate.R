################################################################################
#NAME:         0103_clean_climate.R
#AUTOR:        Alessa Widmaier and co-authors
#DATE:         03/05/2026
#DESCRIPTION:  Clean climate projections from Copernicus
#RUNNING TIME: ~30min
################################################################################


################################################################################
#SECTION 1: INITIATE
################################################################################

#Display starting message
cat(green("\n\nCleaning climate data started."))




################################################################################
#SECTION 2: LOAD DATA
################################################################################

#2.1: Read in required data ----------------------------------------------------

#City-level polygons
cities <- terra::vect("01_data/02_interim/01_cities/cities.gpkg") %>% tidyterra::select(c(city_id, city_name))


#Climate projections
rcp45 <- terra::rast("01_data/01_raw/05_heat/temp_projections/tx_CMIP6_ssp245_mon_201501-210012.nc")
rcp85 <- terra::rast("01_data/01_raw/05_heat/temp_projections/tx_CMIP6_ssp585_mon_201501-210012.nc")




################################################################################
#SECTION 3: CLEAN DATA
################################################################################

#3.1: Define functions ---------------------------------------------------------

#Direct terra to a fast temp dir and show progress
terraOptions(memfrac = 0.6, todisk = TRUE, progress = 1)


#Function to compute annual means
annual_means_from_monthlies <- function(r, years) {
  tt <- time(r)
  if (is.null(tt)) stop("Time coordinate not found in the raster.")
  yr <- as.integer(format(as.Date(tt), "%Y"))
  sel <- yr %in% years
  if (!any(sel)) stop("No layers found for requested years.")
  r_sel  <- r[[which(sel)]]
  yr_sel <- yr[sel]
  r_ann <- tapp(r_sel, index = yr_sel, fun = mean, na.rm = TRUE)
  names(r_ann) <- as.character(sort(unique(yr_sel)))
  r_ann
}


#Function to compute area-weighted mean over polygons
area_weighted_polygon_means <- function(r_layer, cities_sf) {
  vals <- exactextractr::exact_extract(
    r_layer,
    cities_sf,
    fun = function(df, ...) {
      val_col <- setdiff(names(df), c("coverage_fraction", "cell_area"))[1]
      w <- df$coverage_fraction
      if ("cell_area" %in% names(df)) w <- w * df$cell_area
      stats::weighted.mean(df[[val_col]], w, na.rm = TRUE)
    },
    include_cell_area = TRUE,
    summarize_df = TRUE,
    progress = TRUE
  )
  unlist(vals)
}



#3.2: Prepare data -------------------------------------------------------------

#Get cities vector into sf format and reproject to same CRS as temperature projections
cities_sf <- st_as_sf(cities) %>%
  st_make_valid() %>%
  st_transform(crs(rcp45))


#Define identifier columns in output
city_id   <- cities_sf$city_id
city_name <- cities_sf$city_name


#Define scenarios
scn <- list(ssp245 = rcp45, ssp585 = rcp85)


#Initiate file to track results
results <- list()



#3.3: Get annual means for chosen years at city level --------------------------

#Iterate over scenarios
for (s in names(scn)) {
  message("Processing ", s, " ...")
  
  # Annual means for requested years
  r_ann <- annual_means_from_monthlies(scn[[s]], year_choice)
  
  # Crop to cities' extent to speed up extraction
  r_ann <- crop(r_ann, ext(vect(cities_sf)))
  
  # Extract for each requested year (layer names are years as character)
  yrs_available <- intersect(as.character(year_choice), names(r_ann))
  if (length(yrs_available) == 0) stop("None of the requested years are present after aggregation.")
  
  s_df <- lapply(yrs_available, function(yr_chr) {
    vals <- area_weighted_polygon_means(r_ann[[yr_chr]], cities_sf)
    tibble(
      scenario = s,
      year = as.integer(yr_chr),
      city_id = city_id,
      city_name = city_name,
      tas_mean_degC = vals
    )
  }) |>
    bind_rows() |>
    arrange(year)
  
  results[[s]] <- s_df
}

final <- bind_rows(results) |>
  arrange(scenario, year, city_id)



#3.4: Clean --------------------------------------------------------------------

#Pivot wider
final <- final %>%
  dplyr::select(c(city_id, city_name, scenario, year, mean_temp = tas_mean_degC)) %>%
  mutate(scenario = ifelse(scenario == "ssp245", "mean_temp_45", "mean_temp_85")) %>%
  pivot_wider(names_from = c("scenario", "year"), values_from = "mean_temp")




################################################################################
#SECTION 4: SAVE AND CLOSE
################################################################################

#Save cleaned data
saveRDS(final, "01_data/02_interim/05_heat/temperature_projections.rds")


#Remove data that is no longer needed
rm(final)


#Display completion messages
cat(green("\nCleaning climate data complete."))

