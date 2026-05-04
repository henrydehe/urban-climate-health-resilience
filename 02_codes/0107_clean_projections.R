################################################################################
#NAME:         0107_clean_projections.R
#AUTOR:        Alessa Widmaier and co-authors
#DATE:         03/05/2026
#DESCRIPTION:  Tidy country-level projections by risk driver
#RUNNING TIME: <1min
################################################################################


################################################################################
#SECTION 1: INITIATE
################################################################################

#Display starting message
cat(green("\n\nCleaning risk projection data started.\n"))




################################################################################
#SECTION 2: LOAD DATA
################################################################################

#2.1: Read in required data ----------------------------------------------------

#Risk projections by year
files <- list.files("01_data/01_raw/03_health_projections/", pattern = "\\.csv$", full.names = TRUE)
risk <- bind_rows(lapply(files, read_csv), .id = "file_index")


#Baseline heat-related deaths at city level to get country codes
countries <- readRDS("01_data/02_interim/02_health_baseline/city_level_disease.rds")




################################################################################
#SECTION 3: CLEAN DATA
################################################################################

#3.1: Basic clean --------------------------------------------------------------

#Keep only relevant columns
risk <- risk %>%
  dplyr::select(risk = "Risk factor", country_name = "Location", year = "Year", exposure_per_100 = "Value") 


#Rename risk drivers in line with baseline data
risk <- risk %>%
  mutate(risk = recode(risk,
                       "Occupational particulate matter, gases, and fumes" = "Ambient particulate matter pollution", 
                       "High body-mass index in adults" = "High body-mass index", 
                       .default = risk))


#Rename countries in line with baseline data
risk <- risk %>%
  mutate(country_name = recode(country_name,
                              "Bolivia (Plurinational State of)" = "Bolivia",
                              "Brunei Darussalam" = "Brunei",
                              "Congo" = "Republic of the Congo",
                              "Democratic People's Republic of Korea" = "North Korea",
                              "Eswatini" = "Swaziland",
                              "Iran (Islamic Republic of)" = "Iran",
                              "Lao People's Democratic Republic" = "Laos",
                              "Republic of Korea"= "South Korea",
                              "Republic of Moldova" = "Moldova",
                              "Russian Federation" = "Russia",
                              "Sao Tome and Principe" = "São Tomé and Príncipe",
                              "Syrian Arab Republic" = "Syria",
                              "Taiwan (Province of China)" = "Taiwan",
                              "United Republic of Tanzania" = "Tanzania",
                              "United States of America" = "United States",
                              "Venezuela (Bolivarian Republic of)" = "Venezuela",
                              "Viet Nam" = "Vietnam",
                              .default = country_name))
    
     
#Bring in country codes
risk <- countries %>%
  dplyr::select(c(city_id = unique_id, city_name = urban_centre_main_name,
                  country_name, country_code = wb_country_code)) %>%
  distinct() %>% 
  left_join(risk, by = c("country_name"), relationship = "many-to-many") %>%
  dplyr::select(risk, city_id, city_name, year, exposure_per_100) %>%
  arrange(risk, city_id, year)



#3.2: Compute exposure change over time ----------------------------------------

#Compute risk exposure change compared to 2020
risk <- risk %>%
  group_by(risk, city_id, city_name) %>%
  mutate(exposure_wrt_2020 = exposure_per_100 / exposure_per_100[year == 2020]) %>%
  ungroup() %>%
  mutate(exposure_wrt_2020 = as.numeric(exposure_wrt_2020))


#Reformat
risk <- risk %>%
  dplyr::select(-c(exposure_per_100)) %>%
  filter(year != 2020) %>%
  mutate(year = paste0("exposure_multiplier_", year)) %>%
  pivot_wider(names_from = year, values_from = exposure_wrt_2020)
  



################################################################################
#SECTION 4: SAVE AND CLOSE
################################################################################

#Save cleaned data
saveRDS(risk, "01_data/02_interim/03_health_projections/risk_projections.rds")


#Remove data that is no longer needed
rm(files, risk, countries)


#Display completion messages
cat(green("\nCleaning risk projection data complete."))

