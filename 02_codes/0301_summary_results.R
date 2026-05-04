################################################################################
#NAME:         0301_summary_results.R
#AUTOR:        Alessa Widmaier and co-authors
#DATE:         03/05/2026
#DESCRIPTION:  Aggregate health, expenditure, and emissions results at country
#              and income-group level
#RUNNING TIME: <1min
################################################################################


################################################################################
#SECTION 1: INITIATE
################################################################################

#Display starting message
cat(green("\n\nSummarising results started."))



#Create output folder
dir.create("01_data/03_final/supplementary", recursive = TRUE, showWarnings = FALSE)




################################################################################
#SECTION 2: LOAD DATA
################################################################################

#2.1: Read model outputs -------------------------------------------------------

# load the heat death and daly counts
if(run_choice == "main_run"){
  heat_model <- read_rds("01_data/02_interim/05_heat/heat_health_impacts.rds") |> mutate(risk = "Heat")
  air_qual_model <- read_rds("01_data/02_interim/04_air_quality/airqual_health_impacts.rds") |> mutate(risk = "Air Quality")
  wash_model <- read_rds("01_data/02_interim/06_wash/wash_health_impacts.rds")
  lifestyle_model <- read_rds("01_data/02_interim/07_lifestyle/lifestyle_health_impacts.rds")
} else {
  heat_model <- read_rds(paste0("01_data/02_interim/05_heat/sensitivity_analysis/", run_choice, "_heat_health_impacts.rds")) |> mutate(risk = "Heat")
  air_qual_model <- read_rds(paste0("01_data/02_interim/04_air_quality/sensitivity_analysis/", run_choice, "_airqual_health_impacts.rds")) |> mutate(risk = "Air Quality")
  wash_model <- read_rds(paste0("01_data/02_interim/06_wash/sensitivity_analysis/", run_choice, "_wash_health_impacts.rds"))
  lifestyle_model <- read_rds(paste0("01_data/02_interim/07_lifestyle/sensitivity_analysis/", run_choice, "_lifestyle_health_impacts.rds"))
}

other_models <- lst(
  air_qual_model,
  wash_model,
  lifestyle_model
)


#2.2: Read supporting datasets -------------------------------------------------

# OECD share of CO2 emissions for healthcare
oecd_co2_share <- read_csv(
  "01_data/01_raw/08_cobenefits/emissions/co2_share_healthcare.csv"
) |> 
  janitor::remove_empty() |> 
  janitor::clean_names() |> 
  select(country_code = country, co2_share_healthcare = share_of_cf_percent) |> 
  mutate(
    co2_share_healthcare = co2_share_healthcare / 100
  )


# load the ghsl matching data
ghsl_city_populations <- readxl::read_excel(
  "01_data/01_raw/01_cities/GHSL_Cities_DB/ghsl_population_projection.xlsx"
) |>
  mutate(`2020` = as.numeric(`2020`)) |>
  select(
    unique_id,
    country_name,
    wb_country_code,
    world_bank_income_group,
    city_pop_2020 = `2020`
  )


aq_assumed_population_growth <- pop_growth <- readRDS(
  "01_data/02_interim/01_cities/population_growth.rds"
) |> 
  select(-city_name, -population)


# get emissions and health expenditure data
expenditure_and_emissions <- read_csv(
  "01_data/02_interim/08_cobenefits/cntry_healthcare_expenditure_emissions.csv"
) |>
  select(-country_name)

ghsl_centroids_sf <- st_read(
  "01_data/01_raw/01_cities/GHSL_Cities_DB/GHS_UCDB_GLOBE_R2024A.gpkg",
  layer = "UC_centroids"
) |>
  select(
    unique_id = ID_UC_G0,
    latitude = PWCentroidX,
    longitude = PWCentroidY
  )


# bring in total all-cause mortality and morbidity data per country
ihme_totals_all_cause <- list.files(
  "01_data/01_raw/02_health_baseline/ihme_totals/",
  full.names = TRUE
) |>
  map(read_csv) |>
  bind_rows() |>
  janitor::clean_names() |>
  filter(year == 2021) |>
  select(measure, location, val) |>
  mutate(
    wb_country_code = countrycode::countrycode(
      location,
      origin = "country.name.en",
      destination = "wb"
    )
  ) |>
  rename(total_all_cause_country_val = val) |>
  drop_na(wb_country_code) |>
  select(wb_country_code, measure, total_all_cause_country_val)

ihme_total_country_dalys <- ihme_totals_all_cause |>
  filter(measure == "DALYs (Disability-Adjusted Life Years)") |>
  select(wb_country_code, total_all_cause_DALYs = total_all_cause_country_val)


# WPP country population data
wpp_country_pop <- read_csv(
  '01_data/01_raw/01_cities/WPP_country_population/WPP_pop.csv'
) |>
  filter(Variant == "Medium", LocTypeName == "Country/Area", Time == 2021) |>
  select(ISO2_code, Location, PopTotal) |>
  janitor::clean_names() |>
  mutate(
    wb_country_code = countrycode::countrycode(
      iso2_code,
      origin = "iso2c",
      destination = "wb"
    ),
    pop_total = pop_total * 10^3
  ) |>
  drop_na(wb_country_code) |>
  select(wb_country_code, country_pop_2021 = pop_total)




################################################################################
#SECTION 3: AGGREGATE RESULTS
################################################################################

#3.1: Aggregate model outputs ----------------------------------------------

country_level_common <- \(model_output) {
  model_output |>
    left_join(ghsl_city_populations, by = join_by(city_id == unique_id)) |>
    drop_na(wb_country_code) |>
    group_by(
      city_id,
      city_name,
      risk,
      country_name,
      wb_country_code,
      world_bank_income_group,
      measure
    ) |>
    summarise(
      across(
        contains("count_"),
        \(x) sum(x, na.rm = TRUE)
      ),
      city_pop_2020 = first(city_pop_2020)
    ) |> 
    group_by(risk, country_name, wb_country_code, world_bank_income_group, measure) |>
    # sum counts and city populations to country level
    summarise(
      across(
        contains("count_"),
        \(x) sum(x, na.rm = TRUE)
      ),
      city_pop_2020 = sum(city_pop_2020)
    ) |>
    ungroup() |>
    left_join(ihme_totals_all_cause, by = c("wb_country_code", "measure")) |>
    left_join(wpp_country_pop, by = "wb_country_code") |>
    drop_na(wb_country_code)
}


heat_country_lvl <- heat_model |>
  country_level_common() |>
  mutate(
    # calculate the change from adaptation in absolute numbers
    change_45_2030 = count_45_2030_adapt - count_45_2030,
    change_85_2030 = count_85_2030_adapt - count_85_2030,
    # calculate the total all-cause urban mortality and morbidity value
    total_all_cause_urban_val = (city_pop_2020 / country_pop_2021) *
      total_all_cause_country_val,
    # calculate heat as percentage of urban all-cause mortality and morbidity
    across(
      matches("count_([0-9]{2}_2030(_adapt)?|baseline)$"),
      \(x) {
        x / total_all_cause_urban_val
      },
      .names = "{.col}_pct_urban_val"
    )
  )

get_others_country_lvl <- \(model_output) {
  model_output |>
    country_level_common() |>
    mutate(
      change_2030 = count_2030 - count_2030_adapt,
      change_2040 = count_2040 - count_2040_adapt,
      change_2050 = count_2050 - count_2050_adapt,
      total_all_cause_urban_val = (city_pop_2020 / country_pop_2021) *
        total_all_cause_country_val,
      across(
        contains("count_"),
        \(x) x / total_all_cause_urban_val,
        .names = "{.col}_pct_urban_val"
      )
    )
}

others_country_lvl <- other_models |>
  map(get_others_country_lvl)

heat_income_group_lvl <- heat_country_lvl |>
  group_by(risk, world_bank_income_group, measure) |>
  summarise(
    across(
      c(
        matches("count_([0-9]{2}_2030(_adapt)?|baseline)$"),
        "city_pop_2020",
        "country_pop_2021",
        "total_all_cause_country_val",
        "total_all_cause_urban_val"
      ),
      \(x) {
        sum(x, na.rm = TRUE)
      }
    )
  ) |>
  ungroup() |> 
  rename(
    total_deaths_DALYs_region = total_all_cause_country_val,
    total_urban_deaths_DALYs_region = total_all_cause_urban_val,
    total_urban_population = city_pop_2020,
    total_overall_population = country_pop_2021
  )

get_others_income_group_lvl <- \(country_lvl_data) {
  country_lvl_data |>
    group_by(risk, world_bank_income_group, measure) |>
    summarise(
      across(
        c(
          matches("count_(\\d{4}(_adapt)?|baseline)$"),
          "city_pop_2020",
          "country_pop_2021",
          "total_all_cause_country_val",
          "total_all_cause_urban_val"
        ),
        \(x) {
          sum(x, na.rm = TRUE)
        }
      )
    ) |>
    ungroup() |>
    rename(
      total_deaths_DALYs_region = total_all_cause_country_val,
      total_urban_deaths_DALYs_region = total_all_cause_urban_val,
      total_urban_population = city_pop_2020,
      total_overall_population = country_pop_2021
    )
}

others_income_group_lvl <- others_country_lvl |>
  map(get_others_income_group_lvl)

if(run_choice == "main_run"){
  writexl::write_xlsx(list_flatten(c(lst(heat_income_group_lvl),others_income_group_lvl,lst(heat_country_lvl),others_country_lvl)),
                      "01_data/03_final/supplementary/model_summary_by_country_income.xlsx")
} else {
  writexl::write_xlsx(list_flatten(c(lst(heat_income_group_lvl),others_income_group_lvl,lst(heat_country_lvl),others_country_lvl)),
                      paste0("01_data/03_final/supplementary/", run_choice, "_model_summary_by_country_income.xlsx"))
}




################################################################################
#SECTION 4: CREATE SUMMARY TABLES
################################################################################

#4.1: Build heat summary table -------------------------------------------------

summary_table_common <- \(country_lvl_data) {
  country_lvl_data |>
    left_join(ihme_total_country_dalys, by = "wb_country_code") |>
    left_join(
      expenditure_and_emissions,
      by = c("wb_country_code" = "country_code"),
      relationship = "many-to-one"
    ) |>
    left_join(oecd_co2_share, by = join_by(wb_country_code == country_code)) |>
    mutate(
      co2_share_healthcare = if_else(
        is.na(co2_share_healthcare),
        mean(oecd_co2_share$co2_share_healthcare, na.rm = TRUE),
        co2_share_healthcare
      ),
      measure = if_else(
        measure == "DALYs (Disability-Adjusted Life Years)",
        "DALYs",
        measure
      )
    ) |>
    drop_na(co2) |>
    group_by(risk, measure)
}


heat_summary_table <- heat_country_lvl |>
  summary_table_common() |>
  summarise(
    across(
      matches("count_([0-9]{2}_(2030|2040|2050)(_adapt)?|baseline)$"),
      \(x) {
        sum(x, na.rm = TRUE)
      }
    ),
    across(
      matches("count_([0-9]{2}_(2030|2040|2050)_(adapt)?|baseline)_pct_urban_val$"),
      \(x) {
        weighted.mean(x, city_pop_2020, na.rm = TRUE)
      }
    ),
    co2_share_healthcare = weighted.mean(
      co2_share_healthcare,
      co2,
      na.rm = TRUE
    ),
    across(
      c(
        change_45_2030, change_85_2030,
        total_all_cause_DALYs,
        healthcare_expenditure_usd,
        gdp_current_us,
        co2
      ),
      \(x) {
        sum(x, na.rm = TRUE)
      }
    )
  ) |>
  mutate(
    pct_change_45_2030 = change_45_2030 / count_45_2030,
    pct_change_85_2030 = change_85_2030 / count_85_2030,
    DALYs_share_saved_45_2030 = (count_45_2030_adapt[measure == "DALYs"] - count_45_2030[measure == "DALYs"]) / total_all_cause_DALYs,
    DALYs_share_saved_85_2030 = (count_85_2030_adapt[measure == "DALYs"] - count_85_2030[measure == "DALYs"]) / total_all_cause_DALYs,
    DALYs_share_saved_45_2040 = (count_45_2040_adapt[measure == "DALYs"] - count_45_2040[measure == "DALYs"]) / total_all_cause_DALYs,
    DALYs_share_saved_85_2040 = (count_85_2040_adapt[measure == "DALYs"] - count_85_2040[measure == "DALYs"]) / total_all_cause_DALYs,
    DALYs_share_saved_45_2050 = (count_45_2050_adapt[measure == "DALYs"] - count_45_2050[measure == "DALYs"]) / total_all_cause_DALYs,
    DALYs_share_saved_85_2050 = (count_85_2050_adapt[measure == "DALYs"] - count_85_2050[measure == "DALYs"]) / total_all_cause_DALYs,
    spend_health_saved_45_2030 = DALYs_share_saved_45_2030 * healthcare_expenditure_usd,
    spend_health_saved_85_2030 = DALYs_share_saved_85_2030 * healthcare_expenditure_usd,
    spend_health_saved_45_2040 = DALYs_share_saved_45_2040 * healthcare_expenditure_usd,
    spend_health_saved_85_2040 = DALYs_share_saved_85_2040 * healthcare_expenditure_usd,
    spend_health_saved_45_2050 = DALYs_share_saved_45_2050 * healthcare_expenditure_usd,
    spend_health_saved_85_2050 = DALYs_share_saved_85_2050 * healthcare_expenditure_usd,
    co2_health_saved_45_2030 = DALYs_share_saved_45_2030 * co2 * co2_share_healthcare,
    co2_health_saved_85_2030 = DALYs_share_saved_85_2030 * co2 * co2_share_healthcare,
    co2_health_saved_45_2040 = DALYs_share_saved_45_2040 * co2 * co2_share_healthcare,
    co2_health_saved_85_2040 = DALYs_share_saved_85_2040 * co2 * co2_share_healthcare,
    co2_health_saved_45_2050 = DALYs_share_saved_45_2050 * co2 * co2_share_healthcare,
    co2_health_saved_85_2050 = DALYs_share_saved_85_2050 * co2 * co2_share_healthcare,
    share_all_co2_saved_45_2030 = co2_health_saved_45_2030 / co2,
    share_all_co2_saved_85_2030 = co2_health_saved_85_2030 / co2,
    share_all_co2_saved_45_2040 = co2_health_saved_45_2040 / co2,
    share_all_co2_saved_85_2040 = co2_health_saved_85_2040 / co2,
    share_all_co2_saved_45_2050 = co2_health_saved_45_2050 / co2,
    share_all_co2_saved_85_2050 = co2_health_saved_85_2050 / co2
  ) |>
  select(
    -c(
      total_all_cause_DALYs,
      gdp_current_us,
      healthcare_expenditure_usd,
      co2,
      co2_share_healthcare
    )
  ) |>
  pivot_longer(
    cols = -c(risk, measure),
    names_to = "summary_variable",
    values_to = "value"
  ) |>
  pivot_wider(
    names_from = measure,
    values_from = value
  ) |>
  mutate(
    summary_variable = case_match(
      summary_variable,
      "count_baseline" ~ "Total Number (Baseline)",
      "count_45_2030" ~ "Total Number (RCP4.5 2030)",
      "count_45_2030_adapt" ~ "Total Number (RCP4.5 2030) + Adaptation",
      "count_85_2030" ~ "Total Number (RCP8.5 2030)",
      "count_85_2030_adapt" ~ "Total Number (RCP8.5 2030) + Adaptation",
      "count_45_2040" ~ "Total Number (RCP4.5 2040)",
      "count_45_2040_adapt" ~ "Total Number (RCP4.5 2040) + Adaptation",
      "count_85_2040" ~ "Total Number (RCP8.5 2040)",
      "count_85_2040_adapt" ~ "Total Number (RCP8.5 2040) + Adaptation",
      "count_45_2050" ~ "Total Number (RCP4.5 2050)",
      "count_45_2050_adapt" ~ "Total Number (RCP4.5 2050) + Adaptation",
      "count_85_2050" ~ "Total Number (RCP8.5 2050)",
      "count_85_2050_adapt" ~ "Total Number (RCP8.5 2050) + Adaptation",
      "count_baseline_pct_urban_val" ~
        "Baseline Total % of All-Cause Urban Mortality/DALYs",
      "count_45_2030_pct_urban_val" ~
        "RCP4.5 2030 Total % of All-Cause Urban Mortality/DALYs",
      "count_45_2030_adapt_pct_urban_val" ~
        "RCP4.5 2030 + Adaptation Total % of All-Cause Urban Mortality/DALYs",
      "count_85_2030_pct_urban_val" ~
        "RCP8.5 2030 Total % of All-Cause Urban Mortality/DALYs",
      "count_85_2030_adapt_pct_urban_val" ~
        "RCP8.5 2030 + Adaptation Total % of All-Cause Urban Mortality/DALYs",
      "change_45_2030" ~
        "Total reduction in Deaths/DALYs from Adaptation (RCP4.5 2030)",
      "change_85_2030" ~
        "Total reduction in Deaths/DALYs from Adaptation (RCP8.5 2030)",
      "pct_change_45_2030" ~
        "Percentage change in Deaths/DALYs from Adaptation (RCP4.5 2030)",
      "pct_change_85_2030" ~
        "Percentage change in Deaths/DALYs from Adaptation (RCP8.5 2030)",
      "spend_health_saved_45_2030" ~
        "Expenditure Saving from Adaptation (RCP4.5 2030) (USD)",
      "spend_health_saved_85_2030" ~
        "Expenditure Saving from Adaptation (RCP8.5 2030) (USD)",
      "spend_health_saved_45_2040" ~
        "Expenditure Saving from Adaptation (RCP4.5 2040) (USD)",
      "spend_health_saved_85_2040" ~
        "Expenditure Saving from Adaptation (RCP8.5 2040) (USD)",
      "spend_health_saved_45_2050" ~
        "Expenditure Saving from Adaptation (RCP4.5 2050) (USD)",
      "spend_health_saved_85_2050" ~
        "Expenditure Saving from Adaptation (RCP8.5 2050) (USD)",
      "co2_health_saved_45_2030" ~
        "Emissions Reduction from Adaptation (RCP4.5 2030) (MtCO2)",
      "co2_health_saved_85_2030" ~
        "Emissions Reduction from Adaptation (RCP8.5 2030) (MtCO2)",
      "co2_health_saved_45_2040" ~
        "Emissions Reduction from Adaptation (RCP4.5 2040) (MtCO2)",
      "co2_health_saved_85_2040" ~
        "Emissions Reduction from Adaptation (RCP8.5 2040) (MtCO2)",
      "co2_health_saved_45_2050" ~
        "Emissions Reduction from Adaptation (RCP4.5 2050) (MtCO2)",
      "co2_health_saved_85_2050" ~
        "Emissions Reduction from Adaptation (RCP8.5 2050) (MtCO2)",
      "DALYs_share_saved_45_2030" ~
        "Percentage Reduction in Global Healthcare Spending from Adaptation (RCP4.5 2030)",
      "DALYs_share_saved_85_2030" ~
        "Percentage Reduction in Global Healthcare Spending from Adaptation (RCP8.5 2030)",
      "DALYs_share_saved_45_2040" ~
        "Percentage Reduction in Global Healthcare Spending from Adaptation (RCP4.5 2040)",
      "DALYs_share_saved_85_2040" ~
        "Percentage Reduction in Global Healthcare Spending from Adaptation (RCP8.5 2040)",
      "DALYs_share_saved_45_2050" ~
        "Percentage Reduction in Global Healthcare Spending from Adaptation (RCP4.5 2050)",
      "DALYs_share_saved_85_2050" ~
        "Percentage Reduction in Global Healthcare Spending from Adaptation (RCP8.5 2050)",
      "share_all_co2_saved_45_2030" ~
        "Percentage Reduction in Global Emissions from Adaptation (RCP4.5 2030)",
      "share_all_co2_saved_85_2030" ~
        "Percentage Reduction in Global Emissions from Adaptation (RCP8.5 2030)",
      "share_all_co2_saved_45_2040" ~
        "Percentage Reduction in Global Emissions from Adaptation (RCP4.5 2040)",
      "share_all_co2_saved_85_2040" ~
        "Percentage Reduction in Global Emissions from Adaptation (RCP8.5 2040)",
      "share_all_co2_saved_45_2050" ~
        "Percentage Reduction in Global Emissions from Adaptation (RCP4.5 2050)",
      "share_all_co2_saved_85_2050" ~
        "Percentage Reduction in Global Emissions from Adaptation (RCP8.5 2050)"
    )
  )



#4.2: Build air quality, WASH, and lifestyle summary tables ----------------

get_others_summary_table <- \(country_lvl_data){
  country_lvl_data |>
    summary_table_common() |>
    summarise(
      co2_share_healthcare = weighted.mean(
        co2_share_healthcare,
        co2,
        na.rm = TRUE
      ),
      across(
        -c(
          country_name,
          wb_country_code,
          world_bank_income_group,
          city_pop_2020,
          country_pop_2021,
          total_all_cause_country_val,
          total_all_cause_urban_val,
          contains(c("pct", "percent"))
        ),
        \(x) {
          sum(x, na.rm = TRUE)
        }
      ),
      across(
        contains("pct_urban_val"),
        \(x) {
          weighted.mean(x, city_pop_2020, na.rm = TRUE)
        }
      )
    ) |>
    ungroup() |>
    group_by(risk) |>
    mutate(
      pct_change_2030 = change_2030 / count_2030,
      pct_change_2040 = change_2040 / count_2040,
      pct_change_2050 = change_2050 / count_2050,
      DALYs_share_saved_2030 = (count_2030_adapt[measure == "DALYs"] -
        count_2030[measure == "DALYs"]) /
        total_all_cause_DALYs,
      DALYs_share_saved_2040 = (count_2040_adapt[measure == "DALYs"] -
        count_2040[measure == "DALYs"]) /
        total_all_cause_DALYs,
      DALYs_share_saved_2050 = (count_2050_adapt[measure == "DALYs"] -
        count_2050[measure == "DALYs"]) /
        total_all_cause_DALYs,
      spend_health_saved_2030 = DALYs_share_saved_2030 *
        healthcare_expenditure_usd,
      spend_health_saved_2040 = DALYs_share_saved_2040 *
        healthcare_expenditure_usd,
      spend_health_saved_2050 = DALYs_share_saved_2050 *
        healthcare_expenditure_usd,
      co2_health_saved_2030 = DALYs_share_saved_2030 *
        co2 *
        co2_share_healthcare,
      co2_health_saved_2040 = DALYs_share_saved_2040 *
        co2 *
        co2_share_healthcare,
      co2_health_saved_2050 = DALYs_share_saved_2050 *
        co2 *
        co2_share_healthcare,
      share_all_co2_saved_2030 = co2_health_saved_2030 / co2,
      share_all_co2_saved_2040 = co2_health_saved_2040 / co2,
      share_all_co2_saved_2050 = co2_health_saved_2050 / co2
    ) |>
    ungroup() |>
    select(
      -c(
        total_all_cause_DALYs,
        gdp_current_us,
        healthcare_expenditure_usd,
        co2,
        co2_including_luc,
        co2_share_healthcare
      )
    ) |>
    pivot_longer(
      cols = -c(risk, measure),
      names_to = "summary_variable",
      values_to = "value"
    ) |>
    pivot_wider(
      names_from = measure,
      values_from = value
    ) |>
    mutate(
      summary_variable = case_match(
        summary_variable,
        "count_baseline" ~ "Total Number (Baseline)",
        "count_2030" ~ "Total Number 2030 without Adaptation",
        "count_2030_adapt" ~ "Total Number 2030 with Adaptation",
        "count_2040" ~ "Total Number 2040 without Adaptation",
        "count_2040_adapt" ~ "Total Number 2040 with Adaptation",
        "count_2050" ~ "Total Number 2050 without Adaptation",
        "count_2050_adapt" ~ "Total Number 2050 with Adaptation",
        "count_baseline_pct_urban_val" ~
          "Baseline Total % of All-Cause Urban Mortality/DALYs",
        "count_2030_pct_urban_val" ~
          "2030 Total % of All-Cause Urban Mortality/DALYs",
        "count_2030_adapt_pct_urban_val" ~
          "2030 Total % of All-Cause Urban Mortality/DALYs with Adaptation",
        "count_2040_pct_urban_val" ~
          "2040 Total % of All-Cause Urban Mortality/DALYs",
        "count_2040_adapt_pct_urban_val" ~
          "2040 Total % of All-Cause Urban Mortality/DALYs with Adaptation",
        "count_2050_pct_urban_val" ~
          "2050 Total % of All-Cause Urban Mortality/DALYs",
        "count_2050_adapt_pct_urban_val" ~
          "2050 Total % of All-Cause Urban Mortality/DALYs with Adaptation",
        "change_2030" ~ "Total reduction in Deaths/DALYs from Adaptation 2030",
        "change_2040" ~ "Total reduction in Deaths/DALYs from Adaptation 2040",
        "change_2050" ~ "Total reduction in Deaths/DALYs from Adaptation 2050",
        "pct_change_2030" ~
          "Percentage change in Deaths/DALYs from Adaptation 2030",
        "pct_change_2040" ~
          "Percentage change in Deaths/DALYs from Adaptation 2040",
        "pct_change_2050" ~
          "Percentage change in Deaths/DALYs from Adaptation 2050",
        "spend_health_saved_2030" ~
          "Expenditure Saving from Adaptation 2030 (USD)",
        "spend_health_saved_2040" ~
          "Expenditure Saving from Adaptation 2040 (USD)",
        "spend_health_saved_2050" ~
          "Expenditure Saving from Adaptation 2050 (USD)",
        "co2_health_saved_2030" ~
          "Emissions Reduction from Adaptation 2030 (MtCO2)",
        "co2_health_saved_2040" ~
          "Emissions Reduction from Adaptation 2040 (MtCO2)",
        "co2_health_saved_2050" ~
          "Emissions Reduction from Adaptation 2050 (MtCO2)",
        "DALYs_share_saved_2030" ~
          "Percentage Reduction in Global Healthcare Spending from Adaptation 2030",
        "DALYs_share_saved_2040" ~
          "Percentage Reduction in Global Healthcare Spending from Adaptation 2040",
        "DALYs_share_saved_2050" ~
          "Percentage Reduction in Global Healthcare Spending from Adaptation 2050",
        "share_all_co2_saved_2030" ~
          "Percentage Reduction in Global Emissions from Adaptation 2030",
        "share_all_co2_saved_2040" ~
          "Percentage Reduction in Global Emissions from Adaptation 2040",
        "share_all_co2_saved_2050" ~
          "Percentage Reduction in Global Emissions from Adaptation 2050"
      )
    )

}

others_summary_table <- others_country_lvl |> 
  map(get_others_summary_table) |> 
  bind_rows()


  
################################################################################
#SECTION 5: SAVE AND CLOSE
################################################################################

#5.1: Combine and save summary tables --------------------------------------

combined_summary_table <- heat_summary_table |>
  select(risk, summary_variable, Deaths, DALYs) |>
  bind_rows(others_summary_table)


if(run_choice == "main_run"){
  write_csv(combined_summary_table, "01_data/03_final/supplementary/table_1_alternative.csv")
} else {
  write_csv(combined_summary_table, paste0("01_data/03_final/supplementary/", run_choice, "_table_1_alternative.csv"))
}

#Display completion message
cat(green("\nSummarising results complete."))
