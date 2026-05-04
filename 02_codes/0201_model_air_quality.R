################################################################################
#NAME:         0201_model_airqual.R
#AUTOR:        Alessa Widmaier and co-authors
#DATE:         03/05/2026
#DESCRIPTION:  Model pollution-related mortality/morbidity and adaptation impacts
#              of the air quality-improving bundle
#RUNNING TIME: <1min
################################################################################


################################################################################
#SECTION 1: INITIATE
################################################################################

#Display starting message
cat(green("\n\nModelling air quality started."))




################################################################################
#SECTION 2: LOAD DATA
################################################################################

#2.1: Read in required data ----------------------------------------------------

#City-level data
emissions <- readxl::read_excel("01_data/01_raw/01_cities/GHSL_Cities_DB/GHS_UCDB_GLOBE_R2024A.xlsx", sheet = "EMISSIONS")
age <- readxl::read_excel("01_data/01_raw/01_cities/GHSL_Cities_DB/GHS_UCDB_GLOBE_R2024A.xlsx", sheet = "SOCIOECONOMIC")


#Air quality RR data
airqual_rr <- read.csv("01_data/01_raw/04_air_quality/IHME_GBD_2019_RELATIVE_RISKS_PM_Y2020M10D15(Sheet1).csv")


#Existing congestion charge interventions
bl_interv <- readxl::read_excel("01_data/01_raw/04_air_quality/aq_interventions_evidence.xlsx", sheet = "aq_adaptation_baseline")


#Baseline deaths & DALYs at city level
bl_health <- readRDS("01_data/02_interim/02_health_baseline/city_level_disease.rds")


#Population growth and risk evolution
pop_growth <- readRDS("01_data/02_interim/01_cities/population_growth.rds")
exposure_growth <- readRDS("01_data/02_interim/03_health_projections/risk_projections.rds")


#Urban-rural split
urb_rur <- readRDS("01_data/02_interim/04_air_quality/airqual_urban_rural.rds")




################################################################################
#SECTION 3: CLEAN DATA
################################################################################

#3.1: Clean emissions and age data ---------------------------------------------

#Select required air quality data
airqual <- emissions %>%
  rename(pm25 = paste0("EM_PM2_CON_", year_baseline)) %>%
  dplyr::select(city_id = ID_UC_G0, city_name = GC_UCN_MAI_2025, country_name = GC_CNT_GAD_2025, pm25)
rm(emissions)


#Select required age data
age <- age %>%
  rename(share_young = paste0("SC_SEC_PCY_", year_baseline),
         share_adult = paste0("SC_SEC_PCA_", year_baseline),
         share_old = paste0("SC_SEC_PCO_", year_baseline)) %>%
  dplyr::select(city_id = ID_UC_G0, city_name = GC_UCN_MAI_2025, share_young, share_adult, share_old)


#Bring together and tidy (assign age shares based on cities in same country for those cities for which info is missing)
airqual <- airqual %>%
  left_join(age, by = c("city_id", "city_name")) %>%
  mutate(share_young = ifelse(share_young == 0, NA, share_young),
         share_adult = ifelse(share_adult == 0, NA, share_adult),
         share_old = ifelse(share_old == 0, NA, share_old)) %>%
  group_by(country_name) %>%
  mutate(share_young = ifelse(is.na(share_young), mean(share_young, na.rm = TRUE), share_young),
         share_adult = ifelse(is.na(share_adult), mean(share_adult, na.rm = TRUE), share_adult),
         share_old = ifelse(is.na(share_old), mean(share_old, na.rm = TRUE), share_old)) %>%
  ungroup() %>%
  dplyr::select(-c(country_name))
rm(age)



#3.2: Clean air quality RR data ------------------------------------------------

#Tidy
airqual_rr <- airqual_rr[-(1:3),]
airqual_rr <- airqual_rr %>%
  dplyr::select(c(condition = 1, pm25 = 2, rr_all = 5, rr_25 = 6, rr_30 = 7, rr_35 = 8,
                  rr_40 = 9, rr_45 = 10, rr_50 = 11, rr_55 = 12, rr_60 = 13, rr_65 = 14,
                  rr_70 = 15, rr_75 = 16, rr_80 = 17, rr_85 = 18, rr_90 = 19, rr_95 = 20)) %>%
  mutate(across(-c(condition), ~ as.numeric(str_extract(as.character(.), "^\\S+"))))


#Calculate relative risk for age groups (15-64 and 65+)
airqual_rr <- airqual_rr %>%
  mutate(rr_15_64 = (rr_25 + rr_30 + rr_35 + rr_40 + rr_45 + rr_50 + rr_55 + rr_60) / 8,
         rr_15_64 = ifelse(is.na(rr_15_64), rr_all - 1, rr_15_64 - 1),
         rr_65_plus = (rr_65 + rr_70 + rr_75) / 3,                              #Only use 65-80 population as life expectancy tends to be ~80 (so ignore other buckets to avoid oversampling)
         rr_65_plus = ifelse(is.na(rr_65_plus), rr_all - 1, rr_65_plus - 1)) %>%
  dplyr::select(condition, pm25, rr_15_64, rr_65_plus)



#3.3: Get baseline count based on model choice ---------------------------------

#Select correct column
if(air_quality_urban_incidence == "conservative"){
  bl_health <- bl_health %>% rename(count_baseline = city_number_conservative)
} else {
  bl_health <- bl_health %>% rename(count_baseline = city_number_all_aq_urban)
}




################################################################################
#SECTION 4: CALCULTE CITY-LEVEL AIR QUALITY RELATIVE RISKS
################################################################################

#4.1: Get country-level RR figures by condition - baseline --------------------- Note: this is to make the urban/rural adjustment later on

#Clean RR file
pm_rr_values <- airqual_rr %>%
  dplyr::select(c(pm25)) %>%
  distinct() %>%
  arrange(pm25)


#Harmonise share of young/adult/old people across country
airqual_country <- urb_rur %>%
  dplyr::select(c(city_id, city_name, wb_country_code, pop_city, airqual_country)) %>%
  left_join(airqual, by = c("city_id", "city_name")) %>%
  group_by(wb_country_code) %>%
  mutate(share_young = share_young * pop_city/sum(pop_city, na.rm = TRUE),
         share_adult = share_adult * pop_city/sum(pop_city, na.rm = TRUE),
         share_old = share_old * pop_city/sum(pop_city, na.rm = TRUE)) %>%
  summarise(pm25 = mean(airqual_country, na.rm = TRUE),
            share_young = sum(share_young, na.rm = TRUE),
            share_adult = sum(share_adult, na.rm = TRUE),
            share_old = sum(share_old, na.rm = TRUE)) %>%
  as.data.frame() %>%
  ungroup()


#Get bounding values of air pollution between which to interpolate
for(i in 2:nrow(pm_rr_values)){
  pm25_l <- pm_rr_values[i - 1, 1]
  pm25_h <- pm_rr_values[i , 1]
  airqual_country <- airqual_country %>%
    mutate(pm25_low = ifelse(pm25 > pm25_l, pm25_l, pm25_low),
           pm25_high = ifelse(pm25 > pm25_l, pm25_h, pm25_high))
}
  
  
#Bring in relative risks for upper and lower air pollution values and weight by share of adults vs elderly
airqual_country <- airqual_country %>%
  left_join(airqual_rr, by = c("pm25_low" = "pm25"), relationship = "many-to-many") %>%
  mutate(rr_low = rr_15_64 * share_adult / (share_adult + share_old) + rr_65_plus * share_old / (share_adult + share_old)) %>%
  dplyr::select(-c(rr_15_64, rr_65_plus)) %>%
  left_join(airqual_rr, by = c("pm25_high" = "pm25", "condition"), relationship = "many-to-many") %>%
  mutate(rr_high = rr_15_64 * share_adult / (share_adult + share_old) + rr_65_plus * share_old / (share_adult + share_old)) %>%
  dplyr::select(-c(share_young:share_old, rr_15_64, rr_65_plus))


#Interpolate relative risk based on actual air pollution value
airqual_country <- airqual_country %>%
  mutate(airqual_rr_country = rr_low + (rr_high - rr_low)/(pm25_high - pm25_low)*(pm25 - pm25_low)) %>%
  dplyr::select(c(wb_country_code, condition, airqual_rr_country))



#4.2: Get city-level RR figures by condition - WITHOUT adaptation --------------

#Get bounding values of air pollution between which to interpolate
airqual_bl <- airqual
for(i in 2:nrow(pm_rr_values)){
  pm25_l <- pm_rr_values[i - 1, 1]
  pm25_h <- pm_rr_values[i , 1]
  airqual_bl <- airqual_bl %>%
    mutate(pm25_low = ifelse(pm25 > pm25_l, pm25_l, pm25_low),
           pm25_high = ifelse(pm25 > pm25_l, pm25_h, pm25_high))
}


#Bring in relative risks for upper and lower air pollution values and weight by share of adults vs elderly
airqual_bl <- airqual_bl %>%
  left_join(airqual_rr, by = c("pm25_low" = "pm25"), relationship = "many-to-many") %>%
  mutate(rr_low = rr_15_64 * share_adult / (share_adult + share_old) + rr_65_plus * share_old / (share_adult + share_old)) %>%
  dplyr::select(-c(rr_15_64, rr_65_plus)) %>%
  left_join(airqual_rr, by = c("pm25_high" = "pm25", "condition"), relationship = "many-to-many") %>%
  mutate(rr_high = rr_15_64 * share_adult / (share_adult + share_old) + rr_65_plus * share_old / (share_adult + share_old)) %>%
  dplyr::select(-c(share_young:share_old, rr_15_64, rr_65_plus))


#Interpolate relative risk based on actual air pollution value
airqual_bl <- airqual_bl %>%
  mutate(airqual_rr = rr_low + (rr_high - rr_low)/(pm25_high - pm25_low)*(pm25 - pm25_low)) %>%
  dplyr::select(c(city_id, city_name, condition, airqual_rr))



#4.3: Get city-level RR figures by condition - WITH adaptation -----------------

#Get bounding values of air pollution between which to interpolate
airqual_adapt <- airqual %>%
  mutate(pm25 = pm25 * (1 + air_quality_adapt_impact))
for(i in 2:nrow(pm_rr_values)){
  pm25_l <- pm_rr_values[i - 1, 1]
  pm25_h <- pm_rr_values[i , 1]
  airqual_adapt <- airqual_adapt %>%
    mutate(pm25_low = ifelse(pm25 > pm25_l, pm25_l, pm25_low),
           pm25_high = ifelse(pm25 > pm25_l, pm25_h, pm25_high))
}


#Bring in relative risks for upper and lower air pollution values and weight by share of adults vs elderly
airqual_adapt <- airqual_adapt %>%
  left_join(airqual_rr, by = c("pm25_low" = "pm25"), relationship = "many-to-many") %>%
  mutate(rr_low = rr_15_64 * share_adult / (share_adult + share_old) + rr_65_plus * share_old / (share_adult + share_old)) %>%
  dplyr::select(-c(rr_15_64, rr_65_plus)) %>%
  left_join(airqual_rr, by = c("pm25_high" = "pm25", "condition"), relationship = "many-to-many") %>%
  mutate(rr_high = rr_15_64 * share_adult / (share_adult + share_old) + rr_65_plus * share_old / (share_adult + share_old)) %>%
  dplyr::select(-c(share_young:share_old, rr_15_64, rr_65_plus))


#Interpolate relative risk based on actual air pollution value
airqual_adapt <- airqual_adapt %>%
  mutate(airqual_rr_adapt = rr_low + (rr_high - rr_low)/(pm25_high - pm25_low)*(pm25 - pm25_low)) %>%
  dplyr::select(c(city_id, city_name, condition, airqual_rr_adapt))




################################################################################
#SECTION 5: COMPUTE DEATHS AND DALYs OVER TIME
################################################################################

#5.1: Adjust baseline health for urban/rural split -----------------------------

#Get harmonised urban/rural dataset
urb_rur_adj <- urb_rur %>%
  dplyr::select(c(city_id:wb_country_code)) %>%
  left_join(airqual_country, by = c("wb_country_code"), relationship = "many-to-many") %>%
  left_join(airqual_bl, by = c("city_id", "city_name", "condition"), relationship = "many-to-many") %>%
  mutate(rr_wrt_country = airqual_rr / airqual_rr_country,
         condition = ifelse(condition == "Diabetes mellitus type 2", "Diabetes mellitus",
                            ifelse(condition == "Ischaemic heart disease", "Ischemic heart disease", condition)))


#Adjust baseline incidence at city-level based on its relative RR compared to national level
bl_health <- bl_health %>%
  dplyr::select(c(city_id = unique_id, city_name = urban_centre_main_name, country_name,
                  city_share = city_pct_country_pop, 
                  risk = rei, condition = cause, measure, count_baseline)) %>%
  filter(risk == "Ambient particulate matter pollution") %>%
  mutate(count_baseline_country = count_baseline / city_share) %>%
  left_join(urb_rur_adj, by = c("city_id", "city_name", "condition")) %>%
  
  group_by(city_id, city_name, measure) %>%
  mutate(rr_wrt_country_impute = min(rr_wrt_country, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(rr_wrt_country = ifelse(is.na(rr_wrt_country), rr_wrt_country_impute, rr_wrt_country)) %>%
  dplyr::select(-c(rr_wrt_country_impute)) %>%

  mutate(count_baseline_unadj = count_baseline * rr_wrt_country) %>%
  group_by(wb_country_code, condition, measure) %>%
  mutate(count_baseline_country_unadj = sum(count_baseline_unadj),
         count_baseline_adj = ifelse(count_baseline_country_unadj > count_baseline_country, count_baseline_unadj * count_baseline_country / count_baseline_country_unadj, count_baseline_unadj)) %>%
  ungroup() 


#Save urban adjustment information for later use (vulnerability matrix)
urb_rur <- bl_health %>%
  group_by(wb_country_code, measure) %>%
  summarise(count_baseline = sum(count_baseline, na.rm = TRUE),
            count_baseline_adj = sum(count_baseline_adj, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(urb_adjust_factor = count_baseline_adj / count_baseline) %>%
  dplyr::select(-c(count_baseline, count_baseline_adj))



#5.2: Estimate deaths and DALYs with adaptation --------------------------------

#Join relative risk datasets and tidy
airqual <- airqual_bl %>%
  left_join(airqual_adapt, by = c("city_id", "city_name", "condition")) %>%
  mutate(change_rr = (airqual_rr_adapt - airqual_rr) / airqual_rr,
         condition = ifelse(condition == "Diabetes mellitus type 2", "Diabetes mellitus",
                            ifelse(condition == "Ischaemic heart disease", "Ischemic heart disease", condition))) %>%
  dplyr::select(-c(airqual_rr, airqual_rr_adapt))


#Bring together with baseline health data
airqual_death_daly <- bl_health %>%
  dplyr::select(c(city_id, city_name, country_name, risk, condition, measure, count_baseline = count_baseline_adj)) %>%
  left_join(airqual, by = c("city_id", "city_name", "condition")) %>%
  arrange(city_id, condition, measure)


#Fill in change in relative risk for non-modelled conditions using most conservative across other conditions
airqual_death_daly <- airqual_death_daly %>%
  group_by(city_id, city_name, measure) %>%
  mutate(change_rr_impute = max(change_rr, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(change_rr = ifelse(is.na(change_rr), change_rr_impute, change_rr)) %>%
  dplyr::select(-c(change_rr_impute)) %>%
  filter(!is.infinite(change_rr))


#Add population and exposure growth, and get counts for future years with and without adaptation
airqual_death_daly <- airqual_death_daly %>%
  left_join(pop_growth, by = c("city_id", "city_name")) %>%
  left_join(exposure_growth, by = c("city_id", "city_name", "risk"))
  
count <- 0
for(yr in year_choice){
  if(yr != year_baseline){
    airqual_yr <- airqual_death_daly %>%
      rename(pop_multiplier = paste0("pop_multiplier_", yr),
             exposure_multiplier = paste0("exposure_multiplier_", yr)) %>%
      mutate(count_yr = count_baseline * pop_multiplier * exposure_multiplier,
             count_yr_adapt = count_baseline * pop_multiplier * exposure_multiplier * (1 + change_rr)) %>%
      dplyr::select(c(city_id:measure, count_yr, count_yr_adapt))
    colnames(airqual_yr) <- c("city_id", "city_name", "country_name", "risk", "condition", "measure", paste0("count_", yr), paste0("count_", yr, "_adapt"))
    if(count == 0){
      airqual <- airqual_yr
    } else {
      airqual <- airqual %>%
        left_join(airqual_yr, by = c("city_id", "city_name", "country_name", "risk", "condition", "measure"))
    }
    count <- count + 1
  }
}


#Tidy
airqual_death_daly <- airqual_death_daly %>%
  dplyr::select(c(city_id:count_baseline)) %>%
  left_join(airqual, by = c("city_id", "city_name", "country_name", "risk", "condition", "measure")) %>%
  arrange(city_id, risk, measure, condition)


#Set without adaptation = with adaptation for cities with existing congestion charge schemes
airqual_death_daly <- airqual_death_daly %>%
  left_join(bl_interv, by = c("city_name", "country_name")) %>%
  mutate(congestion_charge_in_place = ifelse(is.na(congestion_charge_in_place), "No", congestion_charge_in_place),
         count_2030_adapt = ifelse(congestion_charge_in_place == "Yes", count_2030, count_2030_adapt),
         count_2040_adapt = ifelse(congestion_charge_in_place == "Yes", count_2040, count_2040_adapt),
         count_2050_adapt = ifelse(congestion_charge_in_place == "Yes", count_2050, count_2050_adapt)) %>%
  dplyr::select(-c(country_name, congestion_charge_in_place))
  



################################################################################
#SECTION 6: SAVE AND CLOSE
################################################################################

#Save cleaned data
if(run_choice == "main_run"){
  saveRDS(airqual_death_daly, "01_data/02_interim/04_air_quality/airqual_health_impacts.rds")
  saveRDS(urb_rur, "01_data/02_interim/04_air_quality/airqual_urban_rural_adjustment_factors.rds")
} else {
  saveRDS(airqual_death_daly, paste0("01_data/02_interim/04_air_quality/sensitivity_analysis/", run_choice, "_airqual_health_impacts.rds"))
}


#Remove data that is no longer needed
rm(airqual, airqual_rr, airqual_bl, airqual_adapt, airqual_death_daly)
rm(bl_health, bl_interv, pm_rr_values, pm25_l, pm25_h)


#Display completion messages
cat(green("\nModelling air quality complete."))

