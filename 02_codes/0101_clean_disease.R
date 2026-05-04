################################################################################
#NAME:         0101_clean_disease.R
#AUTOR:        Alessa Widmaier and co-authors
#DATE:         03/05/2026
#DESCRIPTION:  Downscale country-level disease estimates to the city level
#RUNNING TIME: <1min
################################################################################


################################################################################
#SECTION 1: INITIATE
################################################################################

#Display starting message
cat(green("\n\nCleaning disease data started."))




################################################################################
#SECTION 2: LOAD DATA
################################################################################

#2.1: Read in required data ------------------------------------------------

ghsl_city_populations <- readxl::read_excel("01_data/01_raw/01_cities/GHSL_Cities_DB/ghsl_population_projection.xlsx")


wb_income <- readxl::read_excel(
  '01_data/01_raw/01_cities/wb_country_classification/wb_income_classification.xlsx'
) |>
  janitor::clean_names() |>
  select(code, income_group) |> 
  drop_na(income_group)


# import WPP country population data (aligned with GHSL city populations as both are from WrldPop)
wpp_country_pop <- read_csv(
  '01_data/01_raw/01_cities/WPP_country_population/WPP_pop.csv'
) |>
  filter(Variant == "Medium", LocTypeName == "Country/Area", Time == 2021) |>
  mutate(ISO2_code = ifelse(ISO3_code == "NAM", "NA", ISO2_code)) |>            #Manual adjustment for Namibia)
  select(ISO2_code, Location, PopTotal) |>
  janitor::clean_names() |>
  mutate(
    wb_country_code = countrycode::countrycode(
      iso2_code,
      origin = "iso2c",
      destination = "wb",
      relationship = "many-to-many"
    ),
    pop_total = pop_total * 10^3
  ) |>
  drop_na(wb_country_code) |>
  select(wb_country_code, country_pop_2021 = pop_total)


ihme_raw_rei <- list.files(
  "01_data/01_raw/02_health_baseline/ihme_national_numbers",
  full.names = TRUE
) |>
  map(read_csv) |>
  bind_rows() |>
  select(-any_of(c("upper", "lower", "metric", "sex", "age")))


ihme_raw_all_risks <- "01_data/01_raw/02_health_baseline/ihme_level3_totals" |>
  list.files(full.names = TRUE) |>
  map(read_csv) |>
  bind_rows() |>
  select(-any_of(c("upper", "lower", "metric", "sex", "age"))) |> 
  mutate(rei = "All risk factors")

ihme_countries <- ihme_raw_rei |>
  bind_rows(ihme_raw_all_risks) |>
  select(location) |>
  distinct() |>
  mutate(
    location_edited = case_when(
      location == "Portuguese Republic" ~ "Portugal",
      location == "Lebanese Republic" ~ "Lebanon",
      TRUE ~ location
    ),
    country_name_clean = stringi::stri_trans_general(
      location_edited,
      id = "Latin-ASCII"
    ),
    wb_country_code = countrycode::countrycode(
      country_name_clean,
      origin = "country.name",
      destination = "wb",
      warn = FALSE
    )
  ) |>
  select(-country_name_clean, -location_edited) |>
  drop_na(wb_country_code)

ihme_rei <- ihme_raw_rei |>
  left_join(ihme_countries, by = "location") |> 
  drop_na(wb_country_code)
  
ihme_all_risks <- ihme_raw_all_risks |>
  left_join(ihme_countries, by = "location") |> 
  drop_na(wb_country_code)

  
ihme_national_numbers <- ihme_rei |>
  bind_rows(ihme_all_risks)




################################################################################
#SECTION 3: CLEAN DATA
################################################################################

#3.1: Process and harmonise data -----------------------------------------------

city_level_disease <- ghsl_city_populations |>
  drop_na(world_bank_income_group) |>
  select(
    unique_id,
    urban_centre_main_name,
    country_name,
    world_bank_income_group,
    wb_country_code,
    pop_2020 = `2020`
  ) |>
  mutate(
    world_bank_income_group = str_to_lower(world_bank_income_group),
    world_bank_income_group = str_remove_all(world_bank_income_group, "income"),
    world_bank_income_group = str_squish(world_bank_income_group)
  ) |>
  mutate(pop_2020 = as.numeric(pop_2020)) |>
  left_join(wpp_country_pop, by = "wb_country_code") |>
  mutate(city_pct_country_pop = pop_2020 / country_pop_2021) |>
  group_by(wb_country_code) |>
  mutate(
    country_urban_pop = sum(pop_2020, na.rm = TRUE),
    country_urban_pop_pct = country_urban_pop / first(country_pop_2021)
  ) |>
  ungroup() |>
  left_join(
    ihme_national_numbers,
    by = join_by(wb_country_code)
  ) |>
  mutate(val = if_else(val < 0, 0, val)) |>
  mutate(
    city_number_conservative = val * city_pct_country_pop,
    city_number_all_aq_urban = if_else(
      rei %in%
        c("Ambient particulate matter pollution", "Ambient ozone pollution"),
      val * (pop_2020 / country_urban_pop),
      val * city_pct_country_pop
    )
  ) |>
  drop_na(measure)
    



################################################################################
#SECTION 4: SAVE OUTPUTS
################################################################################

#4.1: Save city-level disease outputs ------------------------------------------

write_csv(
  city_level_disease,
  "01_data/02_interim/02_health_baseline/city_level_disease.csv"
)
saveRDS(city_level_disease, "01_data/02_interim/02_health_baseline/city_level_disease.rds")



#4.2: Save country and income-group summaries ----------------------------------

country_level_disease <- city_level_disease |>
  group_by(
    wb_country_code,
    country_name,
    world_bank_income_group,
    rei,
    cause,
    measure
  ) |>
  summarise(
    city_number_conservative = sum(city_number_conservative, na.rm = TRUE),
    pop_2020 = sum(pop_2020, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    country_urban_rate = city_number_conservative / pop_2020 * 100000
  )

income_group_level_disease <- country_level_disease |>
  group_by(world_bank_income_group, rei, cause, measure) |>
  summarise(
    city_number_conservative = sum(city_number_conservative, na.rm = TRUE),
    pop_2020 = sum(pop_2020, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    urban_rate = city_number_conservative / pop_2020 * 100000
  )


writexl::write_xlsx(
  lst(country_level_disease, income_group_level_disease),
  "01_data/02_interim/02_health_baseline/country_and_income_level_disease.xlsx"
)


#Display completion message
cat(green("\nCleaning disease data complete."))
