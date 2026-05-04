################################################################################
#NAME:         0105_clean_green_roofs.R
#AUTOR:        Alessa Widmaier and co-authors
#DATE:         03/05/2026
#DESCRIPTION:  Clean green-roof cooling data
#RUNNING TIME: <1min
################################################################################


################################################################################
#SECTION 1: INITIATE
################################################################################

#Display starting message
cat(green("\n\nCleaning green roof data started."))




################################################################################
#SECTION 2: LOAD DATA
################################################################################

#2.1: Read in GHSL and green-roof cooling data -----------------------------

ghsl_clean <- read_rds("01_data/01_raw/01_cities/GHSL_Cities_DB/ghsl_db.rds")

kg_legend <- tribble(
  ~number, ~code, ~description,
  1,  "Af", "Tropical, rainforest",
  2,  "Am", "Tropical, monsoon",
  3,  "Aw", "Tropical, savannah",
  4,  "BWh", "Arid, desert, hot",
  5,  "BWk", "Arid, desert, cold",
  6,  "BSh", "Arid, steppe, hot",
  7,  "BSk", "Arid, steppe, cold",
  8,  "Csa", "Temperate, dry summer, hot summer",
  9,  "Csb", "Temperate, dry summer, warm summer",
  10,  "Csc", "Temperate, dry summer, cold summer",
  11,  "Cwa", "Temperate, dry winter, hot summer",
  12,  "Cwb", "Temperate, dry winter, warm summer",
  13,  "Cwc", "Temperate, dry winter, cold summer",
  14,  "Cfa", "Temperate, no dry season, hot summer",
  15,  "Cfb", "Temperate, no dry season, warm summer",
  16,  "Cfc", "Temperate, no dry season, cold summer",
  17,  "Dsa", "Cold, dry summer, hot summer",
  18,  "Dsb", "Cold, dry summer, warm summer",
  19,  "Dsc", "Cold, dry summer, cold summer",
  20,  "Dsd", "Cold, dry summer, very cold winter",
  21,  "Dwa", "Cold, dry winter, hot summer",
  22,  "Dwb", "Cold, dry winter, warm summer",
  23,  "Dwc", "Cold, dry winter, cold summer",
  24,  "Dwd", "Cold, dry winter, very cold winter",
  25,  "Dfa", "Cold, no dry season, hot summer",
  26,  "Dfb", "Cold, no dry season, warm summer",
  27,  "Dfc", "Cold, no dry season, cold summer",
  28,  "Dfd", "Cold, no dry season, very cold winter",
  29,  "ET",  "Polar, tundra",
  30,  "EF",  "Polar, frost"
)


# Define territories/special areas that do not map unambiguously to one World Bank country code in the countrycode package.
ambiguous_country_matches <- c(
  "French Guiana" = NA_character_,
  "Jersey" = NA_character_,
  "Martinique" = NA_character_,
  "Mayotte" = NA_character_,
  "Réunion" = NA_character_,
  "Western Sahara" = NA_character_
)

ghsl_kg_current <- ghsl_clean |>
  pluck("CLIMATE") |>
  filter(str_detect(variable, "^CL_KOP.*")) |>
  filter(variable == "CL_KOP_CUR") |>
  select(-c(`1970`:`2030`, `2040`, `2070`)) |>
  mutate(
    country_name = if_else(country_name == "México", "Mexico", country_name),
    wb_country_code = countrycode::countrycode(
      country_name,
      origin = "country.name.en",
      destination = "wb",
      custom_match = ambiguous_country_matches
    ),
    `2025` = suppressWarnings(as.numeric(`2025`))
  ) |>
  left_join(kg_legend, by = join_by(`2025` == number))


cool_roofs <- readxl::read_excel(
  "01_data/01_raw/05_heat/green_roof_cooling_effect.xlsx"
) |> 
  janitor::clean_names() |> 
  select(koppen_geiger_code, median) |> 
  rename(
    kg_code = koppen_geiger_code,
    change_in_rooftop_temp = median
  )




################################################################################
#SECTION 3: CLEAN DATA
################################################################################

#3.1: Attach green-roof cooling assumptions to GHSL data -----------------------

ghsl_cool_roofs <- ghsl_kg_current |>
  left_join(cool_roofs, by = join_by(code == kg_code)) |>
  select(
    unique_id,
    urban_centre_main_name,
    code,
    description,
    change_in_rooftop_temp
  ) |>
  mutate(
    kg_group = case_match(
      code,
      c("Af", "Am", "Aw") ~ "Tropical",
      c("BWh", "BWk", "BSh", "BSk") ~ "Arid",
      c("Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc", "Cfa", "Cfb", "Cfc") ~
        "Temperate",
      c(
        "Dsa", "Dsb", "Dsc", "Dsd",
        "Dwa", "Dwb", "Dwc", "Dwd",
        "Dfa", "Dfb", "Dfc", "Dfd"
      ) ~
        "Continental",
      c("ET", "EF") ~ "Polar"
    )
  )


# Use the minimum cooling effect within each climate group where available. If a climate group has no available cooling estimate, use the overall minimum cooling effect across all groups.
global_min_rooftop_temp <- min(
  ghsl_cool_roofs$change_in_rooftop_temp,
  na.rm = TRUE
)

kg_group_min_rooftop_temp <- ghsl_cool_roofs |>
  group_by(kg_group) |>
  summarise(
    kg_group_min_rooftop_temp = {
      x <- change_in_rooftop_temp
      
      if (all(is.na(x))) {
        NA_real_
      } else {
        min(x, na.rm = TRUE)
      }
    },
    .groups = "drop"
  )

ghsl_cool_roofs <- ghsl_cool_roofs |>
  left_join(kg_group_min_rooftop_temp, by = "kg_group") |>
  mutate(
    change_in_rooftop_temp = coalesce(
      change_in_rooftop_temp,
      kg_group_min_rooftop_temp,
      global_min_rooftop_temp
    )
  ) |>
  select(-kg_group_min_rooftop_temp)




################################################################################
#SECTION 4: SAVE AND CLOSE
################################################################################

#4.1: Save cleaned green-roof cooling data ---------------------------------

write_csv(
  ghsl_cool_roofs,
  "01_data/02_interim/05_heat/ghsl_green_roof_cooling_effect.csv"
)


#Display completion message
cat(green("\nCleaning green roof data complete."))
