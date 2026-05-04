################################################################################
#NAME:         0106_clean_spending_emissions.R
#AUTOR:        Alessa Widmaier and co-authors
#DATE:         03/05/2026
#DESCRIPTION:  Link healthcare expenditure and emissions data
#RUNNING TIME: <1min
################################################################################


################################################################################
#SECTION 1: INITIATE
################################################################################

#Display starting message
cat(green("\n\nCleaning spending and emissions data started."))




################################################################################
#SECTION 2: LOAD DATA
################################################################################

#2.1: Read GHSL country matching data ------------------------------------------

ghsl_clean <- read_rds("01_data/01_raw/01_cities/GHSL_Cities_DB/ghsl_db.rds")



#2.2: Read healthcare expenditure data -----------------------------------------

wb_gdp <- read_csv(
  "01_data/01_raw/08_cobenefits/healthcare_spending/wb_gdp_current_usd.csv",
  skip = 3
) |> 
  janitor::clean_names() |> 
  select(country_name, country_code, indicator_name, x2020) |> 
  pivot_wider(
    names_from = indicator_name,
    values_from = x2020
  ) |> 
  janitor::clean_names()


wb_healthcare_expenditure <- read_csv(
  "01_data/01_raw/08_cobenefits/healthcare_spending/wb_health_exp_pct_gdp.csv",
  skip = 3
) |>
  janitor::clean_names() |>
  select(country_name, country_code, indicator_name, x2020) |>
  pivot_wider(
    names_from = indicator_name,
    values_from = x2020
  ) |>
  janitor::clean_names() |> 
  left_join(wb_gdp, by = c("country_name", "country_code")) |> 
  mutate(
    healthcare_expenditure_usd = current_health_expenditure_percent_of_gdp/100 * gdp_current_us,
  )



#2.3: Read emissions data ------------------------------------------------------

# Define ISO3 codes that do not map unambiguously to World Bank country codes. These were previously converted to NA by countrycode(); we define them explicitly for transparency
ambiguous_iso3c_matches <- c(
  "AIA" = NA_character_,
  "ATA" = NA_character_,
  "BES" = NA_character_,
  "COK" = NA_character_,
  "CXR" = NA_character_,
  "MSR" = NA_character_,
  "NIU" = NA_character_,
  "SHN" = NA_character_,
  "SPM" = NA_character_,
  "VAT" = NA_character_,
  "WLF" = NA_character_
)


owid_emissions <- readxl::read_excel(
  "01_data/01_raw/08_cobenefits/emissions/owid-co2-data.xlsx"
) |>
  janitor::clean_names() |>
  select(country, year, iso_code, co2, co2_including_luc) |> 
  filter(year == 2020) |>
  mutate(wb_country_code = countrycode::countrycode(
    iso_code,
    origin = "iso3c",
    destination = "wb",
    custom_match = ambiguous_iso3c_matches
  )) |> 
  select(wb_country_code, co2, co2_including_luc)




################################################################################
#SECTION 3: TIDY AND CLOSE
################################################################################

#3.1: Tidy and close -----------------------------------------------------------

#Join
healthcare_emissions <- wb_healthcare_expenditure |> 
  left_join(owid_emissions, by = join_by(country_code == wb_country_code))


#Save
write_csv(
  healthcare_emissions,
  "01_data/02_interim/08_cobenefits/cntry_healthcare_expenditure_emissions.csv"
)


#Display completion message
cat(green("\nCleaning spending and emissions data complete."))
