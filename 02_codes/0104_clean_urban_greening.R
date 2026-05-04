################################################################################
#NAME:         0104_clean_urban_greening.R
#AUTOR:        Alessa Widmaier and co-authors
#DATE:         03/05/2026
#DESCRIPTION:  Clean urban greening and tree-cover cooling data
#RUNNING TIME: <1min
################################################################################


################################################################################
#SECTION 1: INITIATE
################################################################################

#Display starting message
cat(green("\n\nCleaning urban greening data started."))




################################################################################
#SECTION 2: LOAD DATA
################################################################################

#2.1: Read and clean GHSL greening data ----------------------------------------

get_ghsl_greening_data <- function() {
    ghsl_clean <- read_rds(
        '01_data/01_raw/01_cities/GHSL_Cities_DB/ghsl_db.rds'
    )
  
  ghsl_general_characteristics <-
    readxl::read_excel(
      "01_data/01_raw/01_cities/GHSL_Cities_DB/GHS_UCDB_GLOBE_R2024A.xlsx",
      sheet = "GENERAL_CHARACTERISTICS"
    ) |>
    select(
      unique_id = ID_UC_G0,
      urban_centre_area_sqkm = GC_UCA_KM2_2025
    )
  
  ghsl_centroids <- st_read(
    "01_data/01_raw/01_cities/GHSL_Cities_DB/GHS_UCDB_GLOBE_R2024A.gpkg",
    layer = "UC_centroids"
  ) |>
    select(
      unique_id = ID_UC_G0,
      latitude = PWCentroidX,
      longitude = PWCentroidY
    ) |> 
    st_drop_geometry()

    ghsl_green_area <- ghsl_clean |>
        pluck("GREENNESS") |>
        filter(str_detect(variable, "GR_SHB_GRN|GR_SQM_GRN")) |>
        janitor::remove_empty("cols") |>
        select(
            unique_id,
            urban_centre_main_name,
            variable_name,
            `2020`
        ) |>
        mutate(`2020` = as.numeric(`2020`)) |>
        pivot_wider(names_from = variable_name, values_from = `2020`) |>
        janitor::clean_names() |>
        select(
            unique_id,
            urban_centre_main_name,
            green_area_pct = share_of_green_area_in_built_up_area,
            green_area_sqm = total_area_of_green_in_built_up_area
        )

    ghsl_built_up_surface <- ghsl_clean |>
        pluck("GHSL") |>
        filter(str_detect(variable, "GH_BUS_TOT")) |>
        janitor::remove_empty("cols") |>
        select(
            unique_id,
            urban_centre_main_name,
            variable_name,
            `2020`
        ) |>
        mutate(`2020` = as.numeric(`2020`)) |>
        pivot_wider(names_from = variable_name, values_from = `2020`) |>
        janitor::clean_names() |>
        select(
            unique_id,
            urban_centre_main_name,
            built_up_surface_sqm = total_built_up_surface,
        )
  
  ghsl_green_area |> 
    left_join(ghsl_built_up_surface, by = c("unique_id", "urban_centre_main_name")) |> 
    left_join(ghsl_general_characteristics, by = "unique_id") |>
    left_join(ghsl_centroids, by = "unique_id")
}


ghsl_data <- get_ghsl_greening_data()




################################################################################
#SECTION 3: CLEAN DATA
################################################################################

#3.1: Extrapolate tree-cover cooling effect values -----------------------------

tce_values <- read_csv("01_data/01_raw/05_heat/tree_cover_cooling_effect.csv") |>
  janitor::clean_names() |>
  select(lon, lat, tce_10_25, tce_20_25, tce_30_25, tce_10_30, tce_10_20) |>
  mutate(
    id = row_number(),
    tce_15_25 = (tce_10_25 + tce_20_25) / 2,
    tce_25_25 = (tce_20_25 + tce_30_25) / 2
  ) |>
  pivot_longer(
    cols = starts_with("tce_"),
    names_to = "tce_variable",
    values_to = "tce_value"
  ) |>
  
  # split the tce_variable into two columns with _
  mutate(
    tree_cover = as.numeric(str_extract(tce_variable, "\\d{2}")),
    air_temperature = as.numeric(str_extract(tce_variable, "\\d{2}$"))
  )

mod <- tce_values |>
  group_by(lon, lat, id) |>
  nest() |>
  mutate(
    lm_model = map(
      data,
      ~ lm(tce_value ~ tree_cover + air_temperature, data = .x)
    )
  ) |> 
  select(-data)

tce_values_expanded <- tce_values |>
  select(-tce_variable) |>
  complete(
    nesting(lon, lat, id),
    tree_cover,
    air_temperature,
    fill = list(tce_value = NA_real_)
  ) |>
  left_join(mod, by = c("lon", "lat", "id")) |>
  rowwise() |> 
  mutate(
    tce_value = if (is.na(tce_value)) {
      as.numeric(
        predict(
          lm_model,
          newdata = tibble(
            tree_cover = tree_cover,
            air_temperature = air_temperature
          )
        )
      )
    } else {
      tce_value
    },
    tce_value = pmax(tce_value, 0)
  ) |>
  ungroup() |>
  select(-lm_model) |>
  mutate(
    tce_variable = paste0("tce_", tree_cover, "_", air_temperature)
  ) |>
  select(-tree_cover, -air_temperature) |>
  pivot_wider(
    names_from = tce_variable,
    values_from = tce_value
  )

  
#3.2: Match cooling-effect values to GHSL cities ---------------------------

  #convert tce_values to sf object
  tce_sf <- tce_values_expanded |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  select(id)
  
  
#Convert GHSL data to spatial format
ghsl_centroids_sf <- st_read(
  "01_data/01_raw/01_cities/GHSL_Cities_DB/GHS_UCDB_GLOBE_R2024A.gpkg",
  layer = "UC_centroids"
) |>
  select(
    unique_id = ID_UC_G0,
    latitude = PWCentroidX,
    longitude = PWCentroidY
  )


#Link GHSL centroids to closest cooling-effect values
ghsl_centroids_matched <- ghsl_centroids_sf |>
  # reproject to WGS 84
  st_transform(crs = 4326) |>
  st_join(tce_sf, join = st_nearest_feature) |>
  select(ghsl_id = unique_id, tce_id = id) |> 
  st_drop_geometry()


ghsl_data_with_tce <- ghsl_data |>
  left_join(ghsl_centroids_matched, by = c("unique_id" = "ghsl_id")) |>
  left_join(tce_values_expanded, by = c("tce_id" = "id")) |> 
  select(-tce_id, -lon, -lat)




################################################################################
#SECTION 4: SAVE AND CLOSE
################################################################################

#4.1: Save cleaned tree-cover cooling data ---------------------------------
write_csv(
  ghsl_data_with_tce,
  "01_data/02_interim/05_heat/ghsl_tree_cover_cooling_effect.csv"
)


#Display completion message
cat(green("\nCleaning urban greening data complete."))
