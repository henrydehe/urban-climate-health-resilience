################################################################################
#NAME:         0203_model_wash.R
#AUTOR:        Alessa Widmaier and co-authors
#DATE:         03/05/2026
#DESCRIPTION:  Model WASH-related mortality/morbidity and adaptation impacts of
#              the WASH-improving bundle
#RUNNING TIME: <1min
################################################################################


################################################################################
#SECTION 1: INITIATE
################################################################################

#Display starting message
cat(green("\n\nModelling WASH started."))


#Reset cli package to read Excel data without problems
Sys.unsetenv("CLI_WIDTH")
options(cli.condition_width = 80)  
options(cli.num_colors = 1, cli.unicode = FALSE)




################################################################################
#SECTION 2: LOAD DATA
################################################################################

#2.1: Read in required data ----------------------------------------------------

#WASH baseline data
water <- readxl::read_excel("01_data/01_raw/06_wash/JMP_2023_WLD.xlsx", sheet = "Water", skip = 2, col_types = "text", na = c("-", ""))
sanitation <- readxl::read_excel("01_data/01_raw/06_wash/JMP_2023_WLD.xlsx", sheet = "Sanitation", skip = 2, col_types = "text", na = c("-", ""))
hygiene <- readxl::read_excel("01_data/01_raw/06_wash/JMP_2023_WLD.xlsx", sheet = "Hygiene", skip = 1, col_types = "text", na = c("-", ""))


#Air quality RR data
wash_rr <- readxl::read_excel("01_data/01_raw/07_lifestyle/IHME_GBD_2019_RELATIVE_RISKS_Y2020M10D15 (3).xlsx", sheet = "Sheet1", skip = 2)


#Baseline deaths & DALYs at city level
bl_health <- readRDS("01_data/02_interim/02_health_baseline/city_level_disease.rds")


#Population growth and risk evolution
pop_growth <- readRDS("01_data/02_interim/01_cities/population_growth.rds")
exposure_growth <- readRDS("01_data/02_interim/03_health_projections/risk_projections.rds")




################################################################################
#SECTION 3: CLEAN DATA
################################################################################

#3.1: Clean WASH data ----------------------------------------------------------

#Water: tidy and define buckets in line with RR data
water <- water %>%
  dplyr::select(c(wb_country_code = 2, year = 3, 
                  unimproved_rural = 8, surfacewater_rural = 9, safely_managed_rural = 21, not_contam_rural = 24, piped_rural = 26,
                  unimproved_urban = 13, surfacewater_urban = 14, safely_managed_urban = 28, not_contam_urban = 31, piped_urban = 33,
                  unimproved_total = 18, surfacewater_total = 19, safely_managed_total = 35, not_contam_total = 38, piped_total = 40)) %>%
  filter(year == 2022) %>%
  dplyr::select(-c(year)) 

water_urban <- water %>%
  mutate(across(c(2:ncol(water)), ~ as.numeric(ifelse(.x == "<1", 0, ifelse(.x == ">99", 100, .x))))) %>%
  mutate(across(c(2:ncol(water)), ~ .x / 100)) %>%
  mutate(unimproved = ifelse(!is.na(unimproved_urban) & !is.na(surfacewater_urban), unimproved_urban + surfacewater_urban, ifelse(!is.na(unimproved_urban), unimproved_urban, surfacewater_urban)),
         unimproved = ifelse(!is.na(unimproved), unimproved, ifelse(!is.na(unimproved_total) & !is.na(surfacewater_total), unimproved_total + surfacewater_total, ifelse(!is.na(unimproved_total), unimproved_total, surfacewater_total))),
         HQ_piped = pmin(safely_managed_urban, not_contam_urban, piped_urban, na.rm = TRUE),
         HQ_piped = ifelse(is.na(HQ_piped), pmin(safely_managed_total, not_contam_total, piped_total, na.rm = TRUE), HQ_piped),
         piped = ifelse(!is.na(piped_urban), piped_urban - HQ_piped, piped_total - HQ_piped),
         improved = 1 - unimproved - HQ_piped - piped) %>%
  dplyr::select(c(wb_country_code, unimproved, improved, piped, HQ_piped))
water_urban[is.na(water_urban)] <- 0
water_urban <- water_urban %>%                                                              #Conservative estimate: if any category is NA, assign missing share to highest quality water
  mutate(difference = 1 - unimproved - improved - piped - HQ_piped,
         HQ_piped = HQ_piped + difference) %>%
  dplyr::select(-c(difference)) %>%
  pivot_longer(cols = c(unimproved:HQ_piped), names_to = "category", values_to = "share_bl")

water_rural <- water %>%
  mutate(across(c(2:ncol(water)), ~ as.numeric(ifelse(.x == "<1", 0, ifelse(.x == ">99", 100, .x))))) %>%
  mutate(across(c(2:ncol(water)), ~ .x / 100)) %>%
  mutate(unimproved = ifelse(!is.na(unimproved_rural) & !is.na(surfacewater_rural), unimproved_rural + surfacewater_rural, ifelse(!is.na(unimproved_rural), unimproved_rural, surfacewater_rural)),
         unimproved = ifelse(!is.na(unimproved), unimproved, ifelse(!is.na(unimproved_total) & !is.na(surfacewater_total), unimproved_total + surfacewater_total, ifelse(!is.na(unimproved_total), unimproved_total, surfacewater_total))),
         HQ_piped = pmin(safely_managed_rural, not_contam_rural, piped_rural, na.rm = TRUE),
         HQ_piped = ifelse(is.na(HQ_piped), pmin(safely_managed_total, not_contam_total, piped_total, na.rm = TRUE), HQ_piped),
         piped = ifelse(!is.na(piped_rural), piped_rural - HQ_piped, piped_total - HQ_piped),
         improved = 1 - unimproved - HQ_piped - piped) %>%
  dplyr::select(c(wb_country_code, unimproved, improved, piped, HQ_piped))
water_rural[is.na(water_rural)] <- 0
water_rural <- water_rural %>%                                                              #Conservative estimate: if any category is NA, assign missing share to highest quality water
  mutate(difference = 1 - unimproved - improved - piped - HQ_piped,
         HQ_piped = HQ_piped + difference) %>%
  dplyr::select(-c(difference)) %>%
  pivot_longer(cols = c(unimproved:HQ_piped), names_to = "category", values_to = "share_bl_rural")


#Sanitation: tidy and define buckets in line with RR data
sanitation <- sanitation %>%
  dplyr::select(c(wb_country_code = 2, year = 3, 
                  unimproved_rural = 8, opendefec_rural = 9, sewer_rural = 31,
                  unimproved_urban = 14, opendefec_urban = 15, sewer_urban = 39,
                  unimproved_total = 20, opendefec_total = 21, sewer_total = 47)) %>%
  filter(year == 2022) %>%
  dplyr::select(-c(year))

sanitation_urban <- sanitation %>%
  mutate(across(c(2:ncol(sanitation)), ~ as.numeric(ifelse(.x == "<1", 0, ifelse(.x == ">99", 100, .x))))) %>%
  mutate(across(c(2:ncol(sanitation)), ~ .x / 100)) %>%
  mutate(unimproved = ifelse(!is.na(unimproved_urban) & !is.na(opendefec_urban), unimproved_urban + opendefec_urban, ifelse(!is.na(unimproved_urban), unimproved_urban, opendefec_urban)),
         unimproved = ifelse(!is.na(unimproved), unimproved, ifelse(!is.na(unimproved_total) & !is.na(opendefec_total), unimproved_total + opendefec_total, ifelse(!is.na(unimproved_total), unimproved_total, opendefec_total))),
         sewer = ifelse(!is.na(sewer_urban), sewer_urban, sewer_total),
         improved = 1 - unimproved - sewer) %>%
  dplyr::select(c(wb_country_code, unimproved, improved, sewer))
sanitation_urban[is.na(sanitation_urban)] <- 0
sanitation_urban <- sanitation_urban %>%                                                    #Conservative estimate: if any category is NA, assign missing share to sewage connection
  mutate(difference = 1 - unimproved - improved - sewer,
         sewer = sewer + difference) %>%
  dplyr::select(-c(difference)) %>%
  pivot_longer(cols = c(unimproved:sewer), names_to = "category", values_to = "share_bl")

sanitation_rural <- sanitation %>%
  mutate(across(c(2:ncol(sanitation)), ~ as.numeric(ifelse(.x == "<1", 0, ifelse(.x == ">99", 100, .x))))) %>%
  mutate(across(c(2:ncol(sanitation)), ~ .x / 100)) %>%
  mutate(unimproved = ifelse(!is.na(unimproved_rural) & !is.na(opendefec_rural), unimproved_rural + opendefec_rural, ifelse(!is.na(unimproved_rural), unimproved_rural, opendefec_rural)),
         unimproved = ifelse(!is.na(unimproved), unimproved, ifelse(!is.na(unimproved_total) & !is.na(opendefec_total), unimproved_total + opendefec_total, ifelse(!is.na(unimproved_total), unimproved_total, opendefec_total))),
         sewer = ifelse(!is.na(sewer_rural), sewer_rural, sewer_total),
         improved = 1 - unimproved - sewer) %>%
  dplyr::select(c(wb_country_code, unimproved, improved, sewer))
sanitation_rural[is.na(sanitation_rural)] <- 0
sanitation_rural <- sanitation_rural %>%                                                    #Conservative estimate: if any category is NA, assign missing share to sewage connection
  mutate(difference = 1 - unimproved - improved - sewer,
         sewer = sewer + difference) %>%
  dplyr::select(-c(difference)) %>%
  pivot_longer(cols = c(unimproved:sewer), names_to = "category", values_to = "share_bl_rural")


#Hygiene: tidy and define buckets in line with RR data
hygiene <- hygiene %>%
  dplyr::select(c(wb_country_code = 2, year = 3, handwashing_rural = 6, handwashing_urban = 10, handwashing_total = 14)) %>%
  filter(year == 2022) %>%
  dplyr::select(-c(year))

hygiene_urban <- hygiene %>%
  mutate(across(c(2:ncol(hygiene)), ~ as.numeric(ifelse(.x == "<1", 0, ifelse(.x == ">99", 100, .x))))) %>%
  mutate(across(c(2:ncol(hygiene)), ~ .x / 100)) %>%
  mutate(handwashing = ifelse(!is.na(handwashing_urban), handwashing_urban, handwashing_total),
         handwashing = ifelse(!is.na(handwashing), handwashing, 1),
         no_handwashing = 1 - handwashing) %>%
  dplyr::select(c(wb_country_code, no_handwashing, handwashing)) %>%
  pivot_longer(cols = c(no_handwashing, handwashing), names_to = "category", values_to = "share_bl")

hygiene_rural <- hygiene %>%
  mutate(across(c(2:ncol(hygiene)), ~ as.numeric(ifelse(.x == "<1", 0, ifelse(.x == ">99", 100, .x))))) %>%
  mutate(across(c(2:ncol(hygiene)), ~ .x / 100)) %>%
  mutate(handwashing = ifelse(!is.na(handwashing_rural), handwashing_rural, handwashing_total),
         handwashing = ifelse(!is.na(handwashing), handwashing, 1),
         no_handwashing = 1 - handwashing) %>%
  dplyr::select(c(wb_country_code, no_handwashing, handwashing)) %>%
  pivot_longer(cols = c(no_handwashing, handwashing), names_to = "category", values_to = "share_bl_rural")



#3.2: Clean WASH RR data -------------------------------------------------------

#Tidy
wash_rr <- wash_rr %>%
  dplyr::select(c(condition = 1, category = 2, rr = 5)) %>%
  mutate(rr = as.numeric(str_extract(as.character(rr), "^\\S+")))


#Split into water, sanitation and hygiene
water_rr <- wash_rr[-(1),]
water_rr <- water_rr[-(13:nrow(water_rr)),]
sanitation_rr <- wash_rr[-(1:14),]
sanitation_rr <- sanitation_rr[-(4:nrow(sanitation_rr)),]
hygiene_rr <- wash_rr[-(1:18),]
hygiene_rr <- hygiene_rr[-(5:nrow(hygiene_rr)),]


#Get harmonised RR for water based on categories available in baseline data (choose most conservative RR score)
water_rr <- water_rr %>%
  mutate(cat1 = ifelse(str_sub(category, 1, 1) == "U", "unimproved",
                       ifelse(str_sub(category, 1, 1) == "I", "improved",
                              ifelse(str_sub(category, 1, 1) == "P", "piped", "HQ_piped"))),
         cat2 = ifelse(str_sub(category, -9, -1) == "untreated", "untreated",
                       ifelse(str_sub(category, -11, -1) == "chlorinated", "chlorinated", "filtered"))) %>%
  dplyr::select(c(condition, cat1, cat2, rr)) %>%
  group_by(condition, cat2) %>%
  mutate(rr_conservative = rr / min(rr)) %>%
  ungroup() %>%
  group_by(condition, cat1) %>%
  summarise(rr = min(rr_conservative)) %>%
  as.data.frame() %>%
  ungroup() %>%
  arrange(rr) %>%
  rename(category = cat1) %>%
  mutate(rr = rr - 1)                                                           #Get excess risk (over and above HQ piped water)


#Tidy sanitation RR
sanitation_rr <- sanitation_rr %>%
  mutate(category = ifelse(category == "Unimproved & untreated", "unimproved", 
                           ifelse(category == "Improved", "improved", "sewer"))) %>%
  mutate(rr = rr - 1)                                                           #Get excess risk (over and above sewer santation)


#Tidy hygiene RR
hygiene_rr <- hygiene_rr %>%
  mutate(category = ifelse(category == "No handwashing w/soap & water", "no_handwashing", "handwashing")) %>%
  mutate(rr = rr - 1)                                                           #Get excess risk (over and above handwashing facilities)



#3.3: Clean baseline health data -----------------------------------------------

#Basic tidy of baseline data
bl_health <- bl_health %>%
  dplyr::select(c(city_id = unique_id, city_name = urban_centre_main_name, wb_country_code,
                  city_pop = pop_2020, urban_pop = country_urban_pop, country_pop = country_pop_2021,
                  risk = rei, condition = cause, measure, count_baseline = city_number_conservative)) %>%
  mutate(condition = ifelse(condition == "Diarrheal diseases", "Diarrhoeal diseases", condition),
         wb_country_code = ifelse(wb_country_code == "TWN", "CHN", wb_country_code)) %>%                           #Use China as proxy for Taiwan as missing from WASH dataset
  arrange(city_id, risk, condition, measure)


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


#Keep only subset of climate-sensitive baseline health impacts
bl_health <- bl_health %>%
  left_join(climate_sens, by = c("city_id", "city_name", "measure", "condition")) %>%
  filter(climate_sensitive == "Yes") %>%
  dplyr::select(-c(climate_sensitive))




################################################################################
#SECTION 4: CALCULTE CITY-LEVEL WASH RELATIVE RISKS WITH AND WITHOUT ADAPTATION
################################################################################

#4.1: Water source -------------------------------------------------------------

#Calculate water source shares with adaptation
water <- water_urban %>%
  left_join(water_rural, by = c("wb_country_code", "category")) %>%
  group_by(wb_country_code) %>%
  mutate(share_adapt = ifelse(category == "unimproved", share_bl - pmin(share_bl, wash_improve_community), share_bl),
         share_adapt = ifelse(category == "improved", share_bl + pmin(share_bl[category == "unimproved"], wash_improve_community), share_adapt),
         share_adapt = ifelse(category == "improved", share_adapt - pmin(share_bl, wash_improve_infra), share_adapt),
         share_adapt = ifelse(category == "piped", share_bl + pmin(share_bl[category == "improved"], wash_improve_infra), share_adapt),
         bottom_quintile = ifelse(category == "unimproved", pmin(share_bl, 0.2), NA),
         bottom_quintile = ifelse(category == "improved", pmin(share_bl, 0.2 - sum(bottom_quintile, na.rm = TRUE)), bottom_quintile),
         bottom_quintile = ifelse(category == "piped", pmin(share_bl, 0.2 - sum(bottom_quintile, na.rm = TRUE)), bottom_quintile),
         bottom_quintile = ifelse(category == "HQ_piped", 0.2 - sum(bottom_quintile, na.rm = TRUE), bottom_quintile),
         bottom_quintile = bottom_quintile / 0.2) %>%
  ungroup()
  

#Calculate weighted RR score for baseline and with adaptation
water <- water %>%
  left_join(water_rr, by = c("category")) %>%
  mutate(product_bl = share_bl * rr,
         product_bl_rural = share_bl_rural * rr,
         product_adapt = share_adapt * rr,
         product_bq = bottom_quintile * rr) %>%
  group_by(wb_country_code, condition) %>%
  summarise(rr_bl = sum(product_bl),
            rr_bl_rural = sum(product_bl_rural),
            rr_adapt = sum(product_adapt),
            rr_bq = sum(product_bq)) %>%
  as.data.frame() %>%
  ungroup() %>%
  mutate(rural_vs_urban_rr = ifelse(is.na(rr_bl_rural / rr_bl), 1, rr_bl_rural/ rr_bl),
         change_rr = ifelse(rr_bl == 0, -1, (rr_adapt - rr_bl) / rr_bl),
         vuln_bq_water = ifelse(rr_bl == 0, 1, rr_bq / rr_bl)) %>%
  dplyr::select(-c(rr_bl, rr_bl_rural, rr_adapt, rr_bq))


#Separate dataframe to track vulnerability of bottom quintile
vuln <- water %>%
  dplyr::select(c(wb_country_code, condition, vuln_bq_water))
water <- water %>%
  dplyr::select(-c(vuln_bq_water))



#4.2: Sanitation ---------------------------------------------------------------

#Calculate sanitation shares with adaptation
sanitation <- sanitation_urban %>%
  left_join(sanitation_rural, by = c("wb_country_code", "category")) %>%
  group_by(wb_country_code) %>%
  mutate(share_adapt = ifelse(category == "unimproved", share_bl - pmin(share_bl, wash_improve_community), share_bl),
         share_adapt = ifelse(category == "improved", share_bl + pmin(share_bl[category == "unimproved"], wash_improve_community), share_adapt),
         share_adapt = ifelse(category == "improved", share_adapt - pmin(share_bl, wash_improve_infra), share_adapt),
         share_adapt = ifelse(category == "sewer", share_bl + pmin(share_bl[category == "improved"], wash_improve_infra), share_adapt),
         bottom_quintile = ifelse(category == "unimproved", pmin(share_bl, 0.2), NA),
         bottom_quintile = ifelse(category == "improved", pmin(share_bl, 0.2 - sum(bottom_quintile, na.rm = TRUE)), bottom_quintile),
         bottom_quintile = ifelse(category == "sewer", 0.2 - sum(bottom_quintile, na.rm = TRUE), bottom_quintile),
         bottom_quintile = bottom_quintile / 0.2) %>%
  ungroup()


#Calculate weighted RR score for baseline and with adaptation
sanitation <- sanitation %>%
  left_join(sanitation_rr, by = c("category")) %>%
  mutate(product_bl = share_bl * rr,
         product_bl_rural = share_bl_rural * rr, 
         product_adapt = share_adapt * rr,
         product_bq = bottom_quintile * rr) %>%
  group_by(wb_country_code, condition) %>%
  summarise(rr_bl = sum(product_bl),
            rr_bl_rural = sum(product_bl_rural),
            rr_adapt = sum(product_adapt),
            rr_bq = sum(product_bq)) %>%
  as.data.frame() %>%
  ungroup() %>%
  mutate(rural_vs_urban_rr = ifelse(is.na(rr_bl_rural / rr_bl), 1, rr_bl_rural/ rr_bl),
         change_rr = ifelse(rr_bl == 0, -1, (rr_adapt - rr_bl) / rr_bl),
         vuln_bq_sanitation = ifelse(rr_bl == 0, 1, rr_bq / rr_bl)) %>%
  dplyr::select(-c(rr_bl, rr_bl_rural, rr_adapt, rr_bq))


#Separate dataframe to track vulnerability of bottom quintile
vuln <- vuln %>%
  full_join(sanitation, by = c("wb_country_code", "condition")) %>%
  dplyr::select(-c(change_rr))
sanitation <- sanitation %>%
  dplyr::select(-c(vuln_bq_sanitation))



#4.3: Hygiene ------------------------------------------------------------------

#Calculate hygiene shares with adaptation
hygiene <- hygiene_urban %>%
  left_join(hygiene_rural, by = c("wb_country_code", "category")) %>%
  group_by(wb_country_code) %>%
  mutate(share_adapt = ifelse(category == "no_handwashing", share_bl - pmin(share_bl, wash_improve_handwash), share_bl),
         share_adapt = ifelse(category == "handwashing", share_bl + pmin(share_bl[category == "no_handwashing"], wash_improve_handwash), share_adapt),
         bottom_quintile = ifelse(category == "no_handwashing", pmin(share_bl, 0.2), NA),
         bottom_quintile = ifelse(category == "handwashing", 0.2 - sum(bottom_quintile, na.rm = TRUE), bottom_quintile),
         bottom_quintile = bottom_quintile / 0.2) %>%
  ungroup()


#Calculate weighted RR score for baseline and with adaptation
hygiene <- hygiene %>%
  left_join(hygiene_rr, by = c("category"), relationship = "many-to-many") %>%
  mutate(product_bl = share_bl * rr,
         product_bl_rural = share_bl_rural * rr,
         product_adapt = share_adapt * rr,
         product_bq = bottom_quintile * rr) %>%
  group_by(wb_country_code, condition) %>%
  summarise(rr_bl = sum(product_bl),
            rr_bl_rural = sum(product_bl_rural),
            rr_adapt = sum(product_adapt),
            rr_bq = sum(product_bq)) %>%
  as.data.frame() %>%
  ungroup() %>%
  mutate(rural_vs_urban_rr = ifelse(is.na(rr_bl_rural / rr_bl), 1, rr_bl_rural/ rr_bl),
         change_rr = ifelse(rr_bl == 0, -1, (rr_adapt - rr_bl) / rr_bl),
         vuln_bq_hygiene = ifelse(rr_bl == 0, 1, rr_bq / rr_bl)) %>%
  dplyr::select(-c(rr_bl, rr_bl_rural, rr_adapt, rr_bq))


#If only community measures or only infrastructure are turned on, divide by two (i.e. assign 50% of handwashing to each community measures and infrastructure)
if (wash_improve_community == 0 | wash_improve_infra == 0){
  hygiene <- hygiene %>%
    mutate(change_rr = ifelse(change_rr == -1, -1, change_rr / 2))
}


#Separate dataframe to track vulnerability of bottom quintile
vuln <- vuln %>%
  full_join(hygiene, by = c("wb_country_code", "condition")) %>%
  dplyr::select(-c(change_rr))
hygiene <- hygiene %>%
  dplyr::select(-c(vuln_bq_hygiene))




################################################################################
#SECTION 5: ESTIMATE DEATHS AND DALYS
################################################################################

#5.1: Adjust baseline estimates for urban/rural split --------------------------

#Join with baseline health data
water_death_daly <- bl_health %>%
  filter(risk == "Unsafe water source") %>%
  left_join(water, by = c("wb_country_code", "condition"))
sanitation_death_daly <- bl_health %>%
  filter(risk == "Unsafe sanitation") %>%
  left_join(sanitation, by = c("wb_country_code", "condition"))
hygiene_death_daly <- bl_health %>%
  filter(risk == "No access to handwashing facility") %>%
  left_join(hygiene, by = c("wb_country_code", "condition"))
death_daly <- rbind(water_death_daly, sanitation_death_daly, hygiene_death_daly)
rm(water_death_daly, sanitation_death_daly, hygiene_death_daly)


#Recalculate city-level incidence rates
death_daly <- death_daly %>%
  mutate(count_baseline_country = count_baseline / city_pop * country_pop) %>%
  mutate(urban_share_of_deaths = ifelse(is.infinite(rural_vs_urban_rr), 0, 
                                        ifelse(rural_vs_urban_rr == 0, 1,
                                               (urban_pop / country_pop) / (rural_vs_urban_rr * (1 - urban_pop / country_pop) + urban_pop / country_pop))),
         count_baseline_adj = count_baseline_country * urban_share_of_deaths * city_pop / urban_pop) 
  

#Save urban adjustment information for later use (vulnerability matrix)
urb_rur <- death_daly %>%
  group_by(wb_country_code, measure) %>%
  summarise(count_baseline = sum(count_baseline, na.rm = TRUE),
            count_baseline_adj = sum(count_baseline_adj, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(urb_adjust_factor = count_baseline_adj / count_baseline) %>%
  dplyr::select(-c(count_baseline, count_baseline_adj))



#5.2: Estimate deaths and DALYs with adaptation --------------------------------

#Add population and exposure growth, and get counts for future years with and without adaptation
death_daly <- death_daly %>%
  dplyr::select(c(city_id, city_name, wb_country_code, risk, condition, measure, count_baseline = count_baseline_adj, change_rr)) %>%
  left_join(pop_growth, by = c("city_id", "city_name")) %>%
  left_join(exposure_growth, by = c("city_id", "city_name", "risk")) %>%
  dplyr::select(-c(wb_country_code)) %>%
  arrange(city_id, risk, condition, measure)
count <- 0
for(yr in year_choice){
  if(yr != year_baseline){
    wash_yr <- death_daly %>%
      rename(pop_multiplier = paste0("pop_multiplier_", yr),
             exposure_multiplier = paste0("exposure_multiplier_", yr)) %>%
      mutate(count_yr = count_baseline * pop_multiplier * exposure_multiplier,
             count_yr_adapt = count_baseline * pop_multiplier * exposure_multiplier * (1 + change_rr)) %>%
      dplyr::select(c(city_id:measure, count_yr, count_yr_adapt))
    colnames(wash_yr) <- c("city_id", "city_name", "risk", "condition", "measure", paste0("count_", yr), paste0("count_", yr, "_adapt"))
    if(count == 0){
      wash <- wash_yr
    } else {
      wash <- wash %>%
        left_join(wash_yr, by = c("city_id", "city_name", "risk", "condition", "measure"))
    }
    count <- count + 1
  }
}


#Tidy
death_daly <- death_daly %>%
  dplyr::select(c(city_id:count_baseline)) %>%
  left_join(wash, by = c("city_id", "city_name", "risk", "condition", "measure")) %>%
  arrange(city_id, risk, condition, measure)
  



################################################################################
#SECTION 6: SAVE AND CLOSE
################################################################################

#Save cleaned data (different files for different combinations of measures to be able to disaggregate waterfall chart)
if(run_choice == "main_run"){
  if(wash_improve_community > 0 & wash_improve_infra > 0){
    saveRDS(death_daly, "01_data/02_interim/06_wash/wash_health_impacts.rds")
  } else if(wash_improve_community > 0){
    saveRDS(death_daly, "01_data/02_interim/06_wash/wash_community_only_health_impacts.rds")
  } else if(wash_improve_infra > 0){
    saveRDS(death_daly, "01_data/02_interim/06_wash/wash_infra_only_health_impacts.rds")
  } else {
    print("No WASH measure defined!")
  }
  saveRDS(vuln, "01_data/02_interim/06_wash/wash_vulnerability_bottom_quintile.rds")
  saveRDS(urb_rur, "01_data/02_interim/06_wash/wash_urban_rural_adjustment_factors.rds")
} else {
  saveRDS(death_daly, paste0("01_data/02_interim/06_wash/sensitivity_analysis/", run_choice, "_wash_health_impacts.rds"))
}


#Remove data that is no longer needed
rm(list = ls(pattern = "wash"), envir = .GlobalEnv)
rm(list = ls(pattern = "water"), envir = .GlobalEnv)
rm(list = ls(pattern = "sanitation"), envir = .GlobalEnv)
rm(list = ls(pattern = "hygiene"), envir = .GlobalEnv)
rm(bl_health, pop_growth)


#Display completion messages
cat(green("\nModelling WASH complete."))

