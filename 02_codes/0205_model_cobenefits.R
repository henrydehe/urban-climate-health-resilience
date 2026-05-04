################################################################################
#NAME:         0205_model_cobenefits.R
#AUTOR:        Alessa Widmaier and co-authors
#DATE:         03/05/2026
#DESCRIPTION:  Model CO2 reductions and revenue from adaptation interventions
#RUNNING TIME: <1min
################################################################################


################################################################################
#SECTION 1: INITIATE
################################################################################

#Display starting message
cat(green("\n\nModelling intervention co-benefits started."))




################################################################################
#SECTION 2: LOAD DATA
################################################################################

#2.1: Read in required data ----------------------------------------------------

#City-level data
pop_area <- readxl::read_excel("01_data/01_raw/01_cities/GHSL_Cities_DB/GHS_UCDB_GLOBE_R2024A.xlsx", sheet = "GHSL")
emissions <- readxl::read_excel("01_data/01_raw/01_cities/GHSL_Cities_DB/GHS_UCDB_GLOBE_R2024A.xlsx", sheet = "EMISSIONS")


#Air quality - impacts of congestion charge
aq_impact <- readxl::read_excel("01_data/01_raw/04_air_quality/aq_interventions_evidence.xlsx", sheet = "aq_adaptation_impacts")


#Existing congestion charge interventions
aq_bl_interv <- readxl::read_excel("01_data/01_raw/04_air_quality/aq_interventions_evidence.xlsx", sheet = "aq_adaptation_baseline")


#Heat -  net sequestration of urban greening and green roofs
heat_sequ <- readxl::read_excel("01_data/01_raw/05_heat/heat_interventions_net_sequestration.xlsx", sheet = "heat_interv_net_sequestration")


#WB income classification for archetypes
archetypes <- readxl::read_excel("01_data/01_raw/01_cities/GHSL_Cities_DB/ghsl_population_projection.xlsx")




################################################################################
#SECTION 3: CLEAN DATA
################################################################################

#3.1: Clean city-level data ----------------------------------------------------

#Clean population and area
pop_area <- pop_area %>%
  dplyr::select(city_id = ID_UC_G0, city_name = GC_UCN_MAI_2025, country_name = GC_CNT_GAD_2025, population = GH_POP_TOT_2020, area = GC_UCA_KM2_2025)


#Clean emissions (CO2 emissions of transport sector only)
emissions <- emissions %>%
  dplyr::select(city_id = ID_UC_G0, city_name = GC_UCN_MAI_2025, country_name = GC_CNT_GAD_2025, co2 = EM_CO2_TRA_2020)



#3.2: Clean intervention data --------------------------------------------------

#Get CO2 impact per m2 from urban greening and green roofs
tree_cover_co2_m2 <- heat_sequ %>% filter(intervention == "tree_cover")
tree_cover_co2_m2 <- as.numeric(mean(tree_cover_co2_m2$co2_impact, na.rm = TRUE))
green_roof_co2_m2 <- heat_sequ %>% filter(intervention == "green_roof")
green_roof_co2_m2 <- as.numeric(mean(green_roof_co2_m2$co2_impact, na.rm = TRUE))


#Get revenue per capita impact from congestion charge intervention
aq_rev_usd_capita <- aq_impact %>%
  dplyr::select(c(city_name, country_name, annual_revenue_usd_2020)) %>%
  left_join(pop_area, by= c("city_name", "country_name")) %>%
  mutate(aq_rev_usd_capita = annual_revenue_usd_2020 / population)
aq_rev_usd_capita <- as.numeric(median(aq_rev_usd_capita$aq_rev_usd_capita, na.rm = TRUE)) #Using median instead of mean to avoid Oslo biasing estimate upwards




################################################################################
#SECTION 4: MODEL CO2 EMISSION REDUCTIONS AND REVENUE FROM INTERVENTIONS
################################################################################

#4.1: Heat ---------------------------------------------------------------------

#Compute area of intervention in m2
heat_co2_savings <- pop_area %>%
  mutate(area_new_tree_cover_m2 = area * 10^6 * tree_cover_increase,
         area_new_green_roof_m2 = area * 10^6 * green_roof_increase,
         tree_cover_co2_savings = area_new_tree_cover_m2 * tree_cover_co2_m2 / 1000,
         green_roof_co2_savings = area_new_green_roof_m2 * green_roof_co2_m2 / 1000,
         heat_co2_savings = tree_cover_co2_savings + green_roof_co2_savings) %>%
  dplyr::select(c(city_id, city_name, heat_co2_savings))



#4.2: Air quality --------------------------------------------------------------

#Compute CO2 emission savings from intervention. Exclude cities with existing congestion charges
aq_co2_savings <- emissions %>%
  mutate(aq_co2_savings = co2 * air_quality_adapt_co2_impact) %>%
  left_join(aq_bl_interv, by = c("city_name", "country_name")) %>%
  mutate(congestion_charge_in_place = ifelse(is.na(congestion_charge_in_place), "No", congestion_charge_in_place),
         aq_co2_savings = ifelse(congestion_charge_in_place == "Yes", 0, aq_co2_savings)) %>%
  dplyr::select(c(city_id, city_name, aq_co2_savings))


#Compute expected annual revenues from intervention. Exclude cities with existing congestion charges
aq_revenue <- pop_area %>%
  mutate(aq_revenue = population * aq_rev_usd_capita) %>%
  left_join(aq_bl_interv, by = c("city_name", "country_name")) %>%
  mutate(congestion_charge_in_place = ifelse(is.na(congestion_charge_in_place), "No", congestion_charge_in_place),
         aq_revenue = ifelse(congestion_charge_in_place == "Yes", 0, aq_revenue)) %>%
  dplyr::select(c(city_id, city_name, aq_revenue))




################################################################################
#SECTION 5: TIDY
################################################################################

#5.1: Join all cobenefits ------------------------------------------------------

#Join
cobenefits <- heat_co2_savings %>%
  left_join(aq_co2_savings, by = c("city_id", "city_name")) %>%
  left_join(aq_revenue, by = c("city_id", "city_name"))

  

#5.2: Get co-benefits by archetype ---------------------------------------------

#Get city-archetype matching
archetypes <- archetypes %>%
  dplyr::select(city_id = unique_id, city_name = urban_centre_main_name, income = world_bank_income_group) %>%
  mutate(city_archetype = recode(income,
                                 "High income" = "established_ageing",
                                 "Upper Middle" = "maturing",
                                 "Lower Middle" = "transitioning",
                                 "Low income" = "fast_growing",
                                 .default = income)) %>%
  dplyr::select(-c(income)) %>%
  distinct()
    

#Sum across all cities within each archetype
cobenefits_archetype <- cobenefits %>%
  left_join(archetypes, by = c("city_id", "city_name")) %>%
  filter(city_archetype != "-") %>%
  group_by(city_archetype) %>%
  summarise(heat_co2_savings = sum(heat_co2_savings),
            aq_co2_savings = sum(aq_co2_savings),
            aq_revenue = sum(aq_revenue)) %>%
  ungroup()


#Save total revenue figure for printing at the end (won't go into the summary tables)
aq_rev_usd_total <- as.numeric(sum(cobenefits_archetype$aq_revenue))/10^9


#Reformat for easy read into summary table
cobenefits_archetype <- cobenefits_archetype %>%
  dplyr::select(-c(aq_revenue)) %>%
  pivot_longer(cols = c(heat_co2_savings, aq_co2_savings), names_to = "risk_driver", values_to = "direct_co2_savings") %>%
  mutate(risk_driver = ifelse(risk_driver == "heat_co2_savings", "Heat", "Air Quality"),
         outcome_metric = "Change in emissions (in MtCO2e) due to adaptation, direct",
         direct_co2_savings = direct_co2_savings / 10^6) %>%
  dplyr::select(c(city_archetype, risk_driver, outcome_metric, direct_co2_savings))
  


#5.3: Get total co-benefits ----------------------------------------------------

#Sum across all cities
cobenefits_total <- cobenefits_archetype %>%
  group_by(risk_driver, outcome_metric) %>%
  summarise(direct_co2_savings = sum(direct_co2_savings))




################################################################################
#SECTION 6: SAVE AND CLOSE
################################################################################

#Print estimated revenue from implementing congestion charges in cities worldwide
paste("Additional to healthcare spending and emission savings, congestion charges can produce",
      "annual revenues in the order of", round(aq_rev_usd_total, 0), "billion USD.")


#Save 
#Save cleaned data
if(run_choice == "main_run"){
  saveRDS(cobenefits, "01_data/02_interim/08_cobenefits/city_global_direct_co2_savings_revenue.rds")
  saveRDS(cobenefits_total, "01_data/02_interim/08_cobenefits/global_direct_co2_savings.rds")
  saveRDS(cobenefits_archetype, "01_data/02_interim/08_cobenefits/archetypes_global_direct_co2_savings.rds")
} else {
  saveRDS(cobenefits, paste0("01_data/02_interim/08_cobenefits/sensitivity_analysis/", run_choice, "_city_global_direct_co2_savings_revenue.rds"))
  saveRDS(cobenefits_total, paste0("01_data/02_interim/08_cobenefits/sensitivity_analysis/", run_choice, "_global_direct_co2_savings.rds"))
  saveRDS(cobenefits_archetype, paste0("01_data/02_interim/08_cobenefits/sensitivity_analysis/", run_choice, "_archetypes_global_direct_co2_savings.rds"))
}


#Remove data that is no longer needed
rm(pop_area, emissions, aq_impact, aq_bl_interv, heat_sequ, archetypes, tree_cover_co2_m2,
   green_roof_co2_m2, aq_rev_usd_capita, heat_co2_savings, aq_co2_savings, 
   aq_revenue, cobenefits, cobenefits_total, aq_rev_usd_total, cobenefits_archetypes)


#Display completion messages
cat(green("\nModelling intervention co-benefits complete."))
