################################################################################
#NAME:         0204_model_lifestyle.R
#AUTOR:        Alessa Widmaier and co-authors
#DATE:         03/05/2026
#DESCRIPTION:  Model lifestyle-related mortality/morbidity and adaptation impacts
#              of the lifestyle-improving bundle
#RUNNING TIME: <1min
################################################################################


################################################################################
#SECTION 1: INITIATE
################################################################################

#Display starting message
cat(green("\n\nModelling lifestyle started."))




################################################################################
#SECTION 2: LOAD DATA
################################################################################

#2.1: Read in required data ----------------------------------------------------

#City-level data
age <- readxl::read_excel("01_data/01_raw/01_cities/GHSL_Cities_DB/GHS_UCDB_GLOBE_R2024A.xlsx", sheet = "SOCIOECONOMIC")


#RR data
rr <- readxl::read_excel("01_data/01_raw/07_lifestyle/IHME_GBD_2019_RELATIVE_RISKS_Y2020M10D15 (3).xlsx", sheet = "Sheet1", skip = 2)


#Baseline deaths & DALYs at city level
bl_health <- readRDS("01_data/02_interim/02_health_baseline/city_level_disease.rds")


#Population growth and risk evolution
pop_growth <- readRDS("01_data/02_interim/01_cities/population_growth.rds")
exposure_growth <- readRDS("01_data/02_interim/03_health_projections/risk_projections.rds")


#Total deaths
deaths <- read.csv("01_data/01_raw/02_health_baseline/ihme_totals/ihme_total_country_deaths.csv")


#Baseline BMI data
bmi <- read.csv("01_data/01_raw/07_lifestyle/NCD_RisC_Lancet_2024_BMI_age_standardised_country.csv")


#Urban-rural split
urb_rur <- read.csv("01_data/01_raw/07_lifestyle/NCD_RisC_2019_rural_urban_mean_BMI_country_2017_wide_by_sex.csv")




################################################################################
#SECTION 3: CLEAN DATA
################################################################################

#3.1: Clean age data -----------------------------------------------------------

#Select required age data
age <- age %>%
  rename(share_young = paste0("SC_SEC_PCY_", year_baseline),
         share_adult = paste0("SC_SEC_PCA_", year_baseline),
         share_old = paste0("SC_SEC_PCO_", year_baseline)) %>%
  dplyr::select(city_id = ID_UC_G0, city_name = GC_UCN_MAI_2025, share_young, share_adult, share_old)



#3.2: Get physical activity RR curve -------------------------------------------

#Tidy
pa_rr <- rr[-(1:2092),]
pa_rr <- pa_rr[-(47:nrow(pa_rr)),]
pa_rr <- pa_rr %>% 
  dplyr::select(c(condition = 1, met = 2, rr_all = 5, rr_25 = 14, rr_30 = 15, 
                  rr_35 = 16, rr_40 = 17, rr_45 = 18, rr_50 = 19, rr_55 = 20, rr_60 = 21, 
                  rr_65 = 22, rr_70 = 23, rr_75 = 24)) %>%
  mutate(across(-c(condition), ~ as.numeric(str_extract(as.character(.), "^\\S+")))) %>%
  arrange(condition, met)


#Get average rr factors by age buckets and rebase
pa_rr <- pa_rr %>%
  mutate(rr_15_64 = rowMeans(pick(rr_25:rr_60), na.rm = TRUE),
         rr_15_64 = ifelse(is.na(rr_15_64), rr_all, rr_15_64),
         rr_65_plus = rowMeans(pick(rr_65:rr_75), na.rm = TRUE),                             
         rr_65_plus = ifelse(is.na(rr_65_plus), rr_all, rr_65_plus)) %>%
  dplyr::select(condition, met, rr_15_64, rr_65_plus) %>%
  filter(met <= 4200) %>%
  group_by(condition) %>%
  mutate(rr_15_64 = rr_15_64 / rr_15_64[met == 4200],
         rr_65_plus = rr_65_plus / rr_65_plus[met == 4200]) %>%
  ungroup()



#3.3: Get BMI RR curve ---------------------------------------------------------

#Tidy
bmi_rr <- rr[-(1:2212),]
bmi_rr <- bmi_rr[-(61:nrow(bmi_rr)),]
bmi_rr <- bmi_rr %>% 
  dplyr::select(c(condition = 1, sex = 4, rr_all = 5, rr_25 = 14, 
                  rr_30 = 15, rr_35 = 16, rr_40 = 17, rr_45 = 18, rr_50 = 19, 
                  rr_55 = 20, rr_60 = 21, rr_65 = 22, rr_70 = 23, rr_75 = 24)) %>%
  mutate(across(-c(condition, sex), ~ as.numeric(str_extract(as.character(.), "^\\S+")))) %>%
  mutate(sex = ifelse(sex == "Males", "male", "female"))


#Average across sexes
bmi_rr <- bmi_rr %>%
  group_by(condition) %>%
  mutate_at(vars(3:ncol(bmi_rr)), mean) %>%
  ungroup() %>%
  dplyr::select(-c(sex)) %>%
  distinct() %>%
  arrange(condition)


#Get average rr factors by age buckets
bmi_rr <- bmi_rr %>%
  mutate(rr_15_64 = rowMeans(pick(rr_25:rr_60), na.rm = TRUE),
         rr_15_64 = ifelse(is.na(rr_15_64), rr_all, rr_15_64),
         rr_65_plus = rowMeans(pick(rr_65:rr_75), na.rm = TRUE),                             
         rr_65_plus = ifelse(is.na(rr_65_plus), rr_all, rr_65_plus)) %>%
  dplyr::select(condition, rr_15_64, rr_65_plus)



#3.4: Clean baseline health data -----------------------------------------------

#Keep only relevant variables and tidy
bl_health <- bl_health %>%
  dplyr::select(c(city_id = unique_id, city_name = urban_centre_main_name, wb_country_code, pop_city = pop_2020, pop_urban = country_urban_pop,
                  pop_country = country_pop_2021, risk = rei, condition = cause, measure, count_baseline = city_number_conservative)) %>%
  arrange(city_id, risk, condition, measure)


#Calculate total deaths / DALYs by cause
totals <- bl_health %>%
  filter(risk == "All risk factors") %>%
  dplyr::select(c(city_id, city_name, condition, measure, count_total = count_baseline))


#Append
bl_health <- bl_health %>%
  left_join(totals, by = c("city_id", "city_name", "condition", "measure"))


#Get list of climate-sensitive conditions
climate_sens <- bl_health %>%
  filter(risk == "High temperature" | risk == "Ambient particulate matter pollution") %>%
  dplyr::select(city_id, city_name, risk, measure, condition, count_baseline) %>%
  group_by(city_id, city_name, risk, measure) %>%
  mutate(share_risk = count_baseline / sum(count_baseline, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(share_risk >= climate_sens_thresh) %>%
  dplyr::select(c(city_id, city_name, measure, condition)) %>%
  distinct() %>%
  arrange(city_id, measure, condition) %>%
  mutate(climate_sensitive = "Yes")



#3.5: Clean baseline BMI data --------------------------------------------------

#Tidy
bmi <- bmi %>%
  filter(Year == 2022) %>%
  dplyr::select(c(wb_country_code = 4, bmi_lt_185 = 5, bmi_185_20 = 17, bmi_20_25 = 20, 
                  bmi_25_30 = 23, bmi_30_35 = 26, bmi_35_40 = 29, bmi_mt_40 = 32)) 


#Average across sexes
bmi <- bmi %>%
  group_by(wb_country_code) %>%
  mutate_at(vars(2:ncol(bmi)), mean) %>%
  ungroup() %>%
  distinct()



#3.6: Tidy urban/rural split ---------------------------------------------------

#Back out relative urban vs rural mean BMI by country
urb_rur <- urb_rur %>%
  mutate(bmi_rural = (bmi_rural_men + bmi_rural_women)/2,
         bmi_urban = (bmi_urban_men + bmi_urban_women)/2,
         bmi_urban_vs_rural = bmi_urban / bmi_rural,
         bmi_urban_vs_rural = ifelse(is.na(bmi_urban_vs_rural), 1, bmi_urban_vs_rural)) %>%
  dplyr::select(c(wb_country_code = iso3, bmi_urban_vs_rural)) 




################################################################################
#SECTION 4: CALCULATE LIFESTYLE LEVELS WITH AND WITHOUT ADAPTATION AT CITY LEVEL
################################################################################

#4.1: Physical activity --------------------------------------------------------

#Derive baseline relative mortality risk at city level
pa <- bl_health %>%
  filter(measure == "Deaths" & risk == "Low physical activity") %>%
  left_join(climate_sens, by = c("city_id", "city_name", "measure", "condition")) %>%
  filter(climate_sensitive == "Yes") %>%
  group_by(city_id, city_name) %>%
  summarise(count_baseline = sum(count_baseline, na.rm = TRUE),
            count_total = sum(count_total, na.rm = TRUE)) %>%
  as.data.frame() %>%
  ungroup() %>%
  mutate(baseline_rr = ifelse(count_total == 0, 0, count_baseline / count_total)) %>%
  dplyr::select(c(city_id, city_name, baseline_rr))


#Rebase baseline_rr in line with asymptote and find on curve and identify baseline METs 
pa <- pa %>%
  mutate(baseline_rr = (baseline_rr + 1) * 0.79,
         met_bl = 452.16 * log(0.21 / (baseline_rr - 0.79)),                    #Use this equation instead of the below as it more closely tracks the average global physical activity of MET = 1000
         #met_bl = 680 * log(0.21 / (baseline_rr - 0.79)),                      
         met_bl = ifelse(is.infinite(met_bl), NA, met_bl),
         met_bl = ifelse(is.na(met_bl), max(met_bl, na.rm = TRUE), met_bl)) %>%
  dplyr::select(-c(baseline_rr))


#Calculate physical activity levels with adaptation
pa <- pa %>%
  filter(!is.na(met_bl)) %>%
  mutate(met_adapt = met_bl + pa_increase)



#4.2: BMI ----------------------------------------------------------------------

#Establish baseline, i.e. get current share of overweight population
bmi <- bmi %>%
  mutate(overweight_bl = rowSums(pick(bmi_25_30:bmi_mt_40), na.rm = TRUE)) %>%
  dplyr::select(c(wb_country_code, overweight_bl, marginally_overweight_bl = bmi_25_30))


#Bring together with cities and conditions
bmi <- bl_health %>%
  dplyr::select(c(city_id, city_name, wb_country_code, condition)) %>%
  distinct() %>%
  left_join(bmi, by = c("wb_country_code")) %>%                                #Note: here we assume same obesity rates in urban and rural areas, which very likely underestimates urban health impacts due to much higher prevalence in urban areas
  dplyr::select(-c(wb_country_code)) %>%
  filter(!is.na(overweight_bl))




################################################################################
#SECTION 5: CALCULTE CITY-LEVEL LIFESTYLE RELATIVE RISKS
################################################################################

#5.1: Physical activity --------------------------------------------------------

#Prep
pa_rr_values <- pa_rr %>%
  dplyr::select(c(met)) %>%
  distinct() %>%
  arrange(met) %>%
  rbind(met = 10000)                                                            #Add very high value to capture exceedances of scale


#Get upper and lower bounds of RR thresholds for baseline and adaptation MET values
for(i in 2:nrow(pa_rr_values)){
  met_l <- as.numeric(pa_rr_values[i - 1, 1])
  met_h <- as.numeric(pa_rr_values[i , 1])
  pa <- pa %>%
    mutate(met_bl_low = ifelse(met_bl > met_l, met_l, met_bl_low),
           met_bl_high = ifelse(met_bl > met_l, met_h, met_bl_high),
           met_bl_high = ifelse(met_bl_high == 10000, met_bl_low, met_bl_high),
           met_adapt_low = ifelse(met_adapt > met_l, met_l, met_adapt_low),
           met_adapt_high = ifelse(met_adapt > met_l, met_h, met_adapt_high),
           met_adapt_high = ifelse(met_adapt_high == 10000, met_adapt_low, met_adapt_high))
}


#Get relative risk for physical activity (based on age shares and interpolated between MET threshold values)
pa <- pa %>%
  left_join(age, by = c("city_id", "city_name")) %>%
  
  #...for baseline physical activity
  left_join(pa_rr, by = c("met_bl_low" = "met"), relationship = "many-to-many") %>%
  mutate(rr_bl_low = rr_15_64 * share_adult / (share_adult + share_old) + rr_65_plus * share_old / (share_adult + share_old)) %>%
  dplyr::select(-c(rr_15_64, rr_65_plus)) %>%
  left_join(pa_rr, by = c("met_bl_high" = "met", "condition"), relationship = "many-to-many") %>%
  mutate(rr_bl_high = rr_15_64 * share_adult / (share_adult + share_old) + rr_65_plus * share_old / (share_adult + share_old)) %>%
  dplyr::select(-c(rr_15_64, rr_65_plus)) %>%
  
  #...for physical activity with adaptation
  left_join(pa_rr, by = c("met_adapt_low" = "met", "condition"), relationship = "many-to-many") %>%
  mutate(rr_adapt_low = rr_15_64 * share_adult / (share_adult + share_old) + rr_65_plus * share_old / (share_adult + share_old)) %>%
  dplyr::select(-c(rr_15_64, rr_65_plus)) %>%
  left_join(pa_rr, by = c("met_adapt_high" = "met", "condition"), relationship = "many-to-many") %>%
  mutate(rr_adapt_high = rr_15_64 * share_adult / (share_adult + share_old) + rr_65_plus * share_old / (share_adult + share_old)) %>%
  dplyr::select(-c(share_young:share_old, rr_15_64, rr_65_plus)) %>%
  
  
  #...interpolate to get RR scores
  mutate(pa_rr_bl = ifelse(met_bl_low == met_bl_high, rr_bl_low, rr_bl_low + (rr_bl_high - rr_bl_low)/(met_bl_high - met_bl_low)*(met_bl - met_bl_low)),
         pa_rr_adapt = ifelse(met_adapt_low == met_adapt_high, rr_adapt_low, rr_adapt_low + (rr_adapt_high - rr_adapt_low)/(met_adapt_high - met_adapt_low)*(met_adapt - met_adapt_low)),
         pa_rr_bl = pa_rr_bl - 1,
         pa_rr_adapt = pa_rr_adapt - 1) %>%
  dplyr::select(c(city_id, city_name, condition, pa_rr_bl, pa_rr_adapt))


#Calculate change in relative risk with adaptation
pa <- pa %>%
  mutate(risk = "Low physical activity",
         change_rr_2030 = (pa_rr_adapt - pa_rr_bl) / pa_rr_bl,
         change_rr_2040 = change_rr_2030,
         change_rr_2050 = change_rr_2030,
         condition = ifelse(condition == "Diabetes mellitus type 2", "Diabetes mellitus",
                            ifelse(condition == "Ischaemic stroke", "Ischemic stroke", condition)),
         change_type = "relative") %>%
  filter(!is.na(condition)) %>%
  dplyr::select(-c(pa_rr_bl, pa_rr_adapt))


#Add in rows for "Chronic kidney disease due to diabetes mellitus type 2"
pa2 <- pa %>%
  filter(condition == "Diabetes mellitus") %>%
  mutate(condition = "Chronic kidney disease")
pa <- rbind(pa, pa2)



#5.2: BMI ----------------------------------------------------------------------

#Align naming                                                                   #CHECK AGAIN THAT NAMING MATCHES WHEN IHME DATA HAS BEEN DOWNLOADED
bmi_rr <- bmi_rr %>%
  mutate(condition = ifelse(condition == "Ischaemic heart disease", "Ischemic heart disease",
                            ifelse(condition == "Ischaemic stroke", "Ischemic stroke", condition)))
  

#Read in relative risk dataset and keep only relevant conditions
bmi <- bmi %>%                                                    
  left_join(bmi_rr, by = c("condition")) %>%
  filter(!is.na(rr_15_64))


#Calculate age-weighted relative risk
bmi <- bmi %>%
  left_join(age, by = c("city_id", "city_name")) %>%
  mutate(rr = rr_15_64 * share_adult / (share_adult + share_old) + rr_65_plus * share_old / (share_adult + share_old),
         rr = pmax(1, rr)) %>%
  dplyr::select(c(city_id:marginally_overweight_bl, rr))


#Estimate change in overweight population
bmi <- bmi %>%
  mutate(overweight_2030_adapt = overweight_bl - overweight_bl * (bmi_decrease_2030 / 5),      #Apply the decrease in BMI only to the portion of overweight people who will drop into the 5-unit band below their current level - very conservative estimate (both because it linearises a likely exponential RR curve and because there are likely more people close to the lower end of each 5-unit bucket than the upper end)
         overweight_2040_adapt = overweight_bl - overweight_bl * (bmi_decrease_2040 / 5), 
         overweight_2050_adapt = overweight_bl - overweight_bl * (bmi_decrease_2050 / 5), 
         rr_bl = rr * overweight_bl + 1 * (1 - overweight_bl),
         rr_2030_adapt = rr * overweight_2030_adapt + 1 * (1 - overweight_2030_adapt),
         rr_2040_adapt = rr * overweight_2040_adapt + 1 * (1 - overweight_2040_adapt),
         rr_2050_adapt = rr * overweight_2050_adapt + 1 * (1 - overweight_2050_adapt),
         risk = "High body-mass index",
         change_rr_2030 = rr_2030_adapt - rr_bl,
         change_rr_2040 = rr_2040_adapt - rr_bl,
         change_rr_2050 = rr_2050_adapt - rr_bl,
         change_type = "absolute") %>%
  dplyr::select(c(city_id, city_name, condition, risk, change_rr_2030:change_type))




################################################################################
#SECTION 6: ESTIMATE DEATHS AND DALYs
################################################################################

#6.1: Estimate deaths and DALYs ------------------------------------------------

#Join change in RR across risks
change_rr <- rbind(pa, bmi)


#Tidy
death_daly <- bl_health %>%
  filter(risk == "Low physical activity" | risk == "High body-mass index") %>%
  left_join(climate_sens, by = c("city_id", "city_name", "measure", "condition"))


#Adjust for urban/rural divide
death_daly <- death_daly %>%
  left_join(urb_rur, by = c("wb_country_code")) %>%
  mutate(count_baseline_country = count_baseline / pop_city * pop_country,
         bmi_urban_vs_rural = ifelse(is.na(bmi_urban_vs_rural), 1, bmi_urban_vs_rural),
         rate_baseline_country_urban = count_baseline_country / (pop_urban + 1 / bmi_urban_vs_rural * (pop_country - pop_urban)),
         count_baseline_adj = pop_city * rate_baseline_country_urban) %>%
  group_by(city_id, city_name, condition) %>%
  mutate(count_total_adj = count_total + sum(count_baseline_adj) - sum(count_baseline)) %>%
  ungroup() 


#Save urban adjustment information for later use (vulnerability matrix) - this adjustment factor INCLUDES reduction from looking at climate-sensitive conditions only
urb_rur <- death_daly %>%
  group_by(wb_country_code, measure, climate_sensitive) %>%
  summarise(count_baseline = sum(count_baseline, na.rm = TRUE),
            count_baseline_adj = sum(count_baseline_adj, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(wb_country_code, measure) %>%
  mutate(count_baseline = sum(count_baseline, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(climate_sensitive == "Yes") %>%
  mutate(urb_adjust_factor = count_baseline_adj / count_baseline) %>%
  dplyr::select(-c(climate_sensitive, count_baseline, count_baseline_adj))
  
  
#Bring in adaptation effect and fill relative risk for non-modelled conditions with most conservative value  
death_daly <- death_daly %>%
  filter(climate_sensitive == "Yes") %>%
  dplyr::select(c(city_id, city_name, risk, condition, measure, count_baseline = count_baseline_adj, count_total = count_total_adj)) %>%
  left_join(change_rr, by = c("city_id", "city_name", "risk", "condition")) %>%
  group_by(city_id, city_name, risk, measure) %>%
  mutate(change_rr_impute_2030 = max(change_rr_2030, na.rm = TRUE, warning = FALSE),
         change_rr_impute_2040 = max(change_rr_2040, na.rm = TRUE, warning = FALSE),
         change_rr_impute_2050 = max(change_rr_2050, na.rm = TRUE, warning = FALSE)) %>%
  ungroup() %>%
  mutate(change_rr_2030 = ifelse(is.na(change_rr_2030), change_rr_impute_2030, change_rr_2030),
         change_rr_2040 = ifelse(is.na(change_rr_2040), change_rr_impute_2040, change_rr_2040),
         change_rr_2050 = ifelse(is.na(change_rr_2050), change_rr_impute_2050, change_rr_2050),
         change_type = ifelse(is.na(change_type), ifelse(risk == "High body-mass index", "absolute", "relative"), change_type),
         count_total = ifelse(is.na(count_total), count_baseline, count_total)) %>%
  dplyr::select(-c(change_rr_impute_2030:change_rr_impute_2050))


#Add population growth and get counts for future years with and without adaptation
death_daly <- death_daly %>%
  left_join(pop_growth, by = c("city_id", "city_name")) %>%
  left_join(exposure_growth, by = c("city_id", "city_name", "risk")) %>%
  filter(!is.na(count_baseline))
count <- 0
for(yr in year_choice){
  if(yr != year_baseline){
    death_daly_yr <- death_daly %>%
      rename(pop_multiplier = paste0("pop_multiplier_", yr),
             exposure_multiplier = paste0("exposure_multiplier_", yr),
             change_rr = paste0("change_rr_", yr)) %>%
      mutate(count_yr = count_baseline * pop_multiplier * exposure_multiplier,
             count_yr_adapt = ifelse(change_type == "relative", count_baseline * pop_multiplier * exposure_multiplier * (1 + change_rr),
                                     (count_baseline + change_rr * count_total) * pop_multiplier * exposure_multiplier)) %>%
      dplyr::select(c(city_id:measure, count_yr, count_yr_adapt))
    colnames(death_daly_yr) <- c("city_id", "city_name", "risk", "condition", "measure", paste0("count_", yr), paste0("count_", yr, "_adapt"))
    if(count == 0){
      death_daly_all <- death_daly_yr
    } else {
      death_daly_all <- death_daly_all %>%
        left_join(death_daly_yr, by = c("city_id", "city_name", "risk", "condition", "measure"))
    }
    count <- count + 1
  }
}


#Tidy
death_daly <- death_daly %>%
  dplyr::select(c(city_id:count_baseline)) %>%
  left_join(death_daly_all, by = c("city_id", "city_name", "risk", "condition", "measure")) %>%
  arrange(city_id, risk, condition, measure)
  



################################################################################
#SECTION 7: SAVE AND CLOSE
################################################################################

#Save cleaned data
if(run_choice == "main_run"){
  saveRDS(death_daly, "01_data/02_interim/07_lifestyle/lifestyle_health_impacts.rds")
  saveRDS(urb_rur, "01_data/02_interim/07_lifestyle/lifestyle_urban_rural_adjustment_factors.rds")
} else {
  saveRDS(death_daly, paste0("01_data/02_interim/07_lifestyle/sensitivity_analysis/", run_choice, "_lifestyle_health_impacts.rds"))
}


#Remove data that is no longer needed
rm(bl_health, met_l, met_h)


#Display completion message
cat(green("\nModelling lifestyle complete."))
