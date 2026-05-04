################################################################################
#NAME:         0303_generate_figures.R
#AUTOR:        Alessa Widmaier and co-authors
#DATE:         03/05/2026
#DESCRIPTION:  Generate tables and figures for the academic article
#RUNNING TIME: <1min
################################################################################


################################################################################
#SECTION 1: INITIATE
################################################################################

#Display starting message
cat(green("\n\nGenerating figures started."))


#Create output folders
dir.create("01_data/03_final", recursive = TRUE, showWarnings = FALSE)
dir.create("01_data/03_final/supplementary", recursive = TRUE, showWarnings = FALSE)




################################################################################
#SECTION 2: LOAD DATA
################################################################################

#2.1: Read in required data for summary table ----------------------------------

#Global summary table
if(run_choice == "main_run"){
  global <- read.csv("01_data/03_final/supplementary/table_1_alternative.csv")
  direct_co2_savings <- readRDS("01_data/02_interim/08_cobenefits/global_direct_co2_savings.rds")
} else {
  global <- read.csv(paste0("01_data/03_final/supplementary/", run_choice, "_table_1_alternative.csv"))
  direct_co2_savings <- readRDS(paste0("01_data/02_interim/08_cobenefits/sensitivity_analysis/", run_choice, "_global_direct_co2_savings.rds"))
}



#2.2: Read in required data for heat map ---------------------------------------

#Summary tables by country
if(run_choice == "main_run"){
  heat_country <- read_excel("01_data/03_final/supplementary/model_summary_by_country_income.xlsx", sheet = "heat_country_lvl")
  aq_country <- read_excel("01_data/03_final/supplementary/model_summary_by_country_income.xlsx", sheet = "air_qual_model_1")
  wash_country <- read_excel("01_data/03_final/supplementary/model_summary_by_country_income.xlsx", sheet = "wash_model_1")
  lifestyle_country <- read_excel("01_data/03_final/supplementary/model_summary_by_country_income.xlsx", sheet = "lifestyle_model_1")
} else {
  heat_country <- read_excel(paste0("01_data/03_final/supplementary/", run_choice, "_model_summary_by_country_income.xlsx"), sheet = "heat_country_lvl")
  aq_country <- read_excel(paste0("01_data/03_final/supplementary/", run_choice, "_model_summary_by_country_income.xlsx"), sheet = "air_qual_model_1")
  wash_country <- read_excel(paste0("01_data/03_final/supplementary/", run_choice, "_model_summary_by_country_income.xlsx"), sheet = "wash_model_1")
  lifestyle_country <- read_excel(paste0("01_data/03_final/supplementary/", run_choice, "_model_summary_by_country_income.xlsx"), sheet = "lifestyle_model_1")
}


#World map approved by World Bank
world_map <- terra::vect("01_data/01_raw/01_cities/country_boundaries/World Bank Official Boundaries - Admin 0.gpkg")




################################################################################
#SECTION 3: GET FIGURES
################################################################################

#3.1: Generate global summary table --------------------------------------------

#Keep RCP4.5 for heat and align with naming across other risk drivers
tidy <- global %>%
  mutate(summary_variable = ifelse(summary_variable == "Total Number (RCP4.5 2030)", "Total Number 2030 without Adaptation", 
                                 ifelse(summary_variable == "Total Number (RCP4.5 2030) + Adaptation", "Total Number 2030 with Adaptation", 
                                        ifelse(summary_variable == "Expenditure Saving from Adaptation (RCP4.5 2030) (USD)", "Expenditure Saving from Adaptation 2030 (USD)",
                                               ifelse(summary_variable == "Emissions Reduction from Adaptation (RCP4.5 2030) (MtCO2)", "Emissions Reduction from Adaptation 2030 (MtCO2)",
                                               
                                                      ifelse(summary_variable == "Total Number (RCP4.5 2050)", "Total Number 2050 without Adaptation", 
                                                             ifelse(summary_variable == "Total Number (RCP4.5 2050) + Adaptation", "Total Number 2050 with Adaptation", 
                                                                    ifelse(summary_variable == "Expenditure Saving from Adaptation (RCP4.5 2050) (USD)", "Expenditure Saving from Adaptation 2050 (USD)", 
                                                                           ifelse(summary_variable == "Emissions Reduction from Adaptation (RCP4.5 2050) (MtCO2)", "Emissions Reduction from Adaptation 2050 (MtCO2)", summary_variable))))))))) %>%


#Keep relevant rows only
  filter(summary_variable == "Total Number (Baseline)" | summary_variable == "Total Number 2030 without Adaptation" | summary_variable == "Total Number 2030 with Adaptation" |
           summary_variable == "Total Number 2050 without Adaptation" | summary_variable == "Total Number 2050 with Adaptation" | summary_variable == "Expenditure Saving from Adaptation 2030 (USD)" |
           summary_variable == "Expenditure Saving from Adaptation 2050 (USD)" | summary_variable == "Emissions Reduction from Adaptation 2030 (MtCO2)" | summary_variable == "Emissions Reduction from Adaptation 2050 (MtCO2)") %>%


#Summarise across WASH and lifestyle factors
  mutate(risk_driver = ifelse(risk == "No access to handwashing facility" | risk == "Unsafe sanitation" | risk == "Unsafe water source", "WASH",
                              ifelse(risk == "High body-mass index" | risk == "Low physical activity", "Lifestyle", risk))) %>%
  group_by(risk_driver, summary_variable) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE),
            DALYs = sum(DALYs, na.rm = TRUE)) %>%
  as.data.frame() %>%
  ungroup() %>%
  mutate(DALYs = ifelse(summary_variable == "Emissions Reduction from Adaptation 2030 (MtCO2)" | summary_variable == "Emissions Reduction from Adaptation 2050 (MtCO2)" |
                          summary_variable == "Expenditure Saving from Adaptation 2030 (USD)" | summary_variable == "Expenditure Saving from Adaptation 2050 (USD)", Deaths, DALYs)) %>%
  
  
#Pivot long format
  pivot_longer(cols = c("Deaths", "DALYs"), names_to = "outcome_metric", values_to = "value") %>%


#Get separate outcome metrics for emission and expenditure savings
  mutate(outcome_metric = ifelse(str_sub(summary_variable, 1, 9) == "Emissions", "Emissions",
                                 ifelse(str_sub(summary_variable, 1, 11) == "Expenditure", "Expenditure", outcome_metric))) %>%
  distinct() %>%
  mutate(summary_variable = ifelse(summary_variable == "Emissions Reduction from Adaptation 2030 (MtCO2)" | summary_variable == "Expenditure Saving from Adaptation 2030 (USD)", "Total Number 2030 with Adaptation",
                                   ifelse(summary_variable == "Emissions Reduction from Adaptation 2050 (MtCO2)" | summary_variable == "Expenditure Saving from Adaptation 2050 (USD)", "Total Number 2050 with Adaptation", summary_variable))) %>%
  
  
#Get into correct units
  mutate(value = ifelse(outcome_metric == "Expenditure", value * 10^(-9),
                        ifelse(outcome_metric == "Deaths" | outcome_metric == "DALYs", value * 10^(-6), value)),
         outcome_metric = ifelse(outcome_metric == "Emissions", "Change in emissions (in MtCO2e) due to adaptation, indirect",
                                 ifelse(outcome_metric == "Expenditure", "Change in healthcare spending (in billion USD) due to adaptation",
                                        ifelse(outcome_metric == "Deaths", "Deaths (in millions)", "DALYs (in millions)")))) %>%
  
  
#Pivot wider format
  pivot_wider(names_from = "summary_variable", values_from = "value") %>%
  

#Rename columns
  dplyr::select(c(risk_driver, outcome_metric, baseline_2021 = "Total Number (Baseline)", no_adaptation_2030 = "Total Number 2030 without Adaptation",
                  adaptation_2030 = "Total Number 2030 with Adaptation", no_adaptation_2050 = "Total Number 2050 without Adaptation", adaptation_2050 = "Total Number 2050 with Adaptation")) %>%
  arrange(risk_driver, desc(outcome_metric))
  
  
#Add in direct CO2 savings from urban greening/green roofs for heat and congestion charges for air quality
direct_co2_savings <- direct_co2_savings %>%
  mutate(baseline_2021 = NA, 
         no_adaptation_2030 = NA, 
         adaptation_2030 = direct_co2_savings, 
         no_adaptation_2050 = NA, 
         adaptation_2050 = direct_co2_savings,
         outcome_metric = "Change in femissions (in MtCO2e) due to adaptation, direct") %>%
  dplyr::select(-c(direct_co2_savings))
tidy <- rbind(tidy, direct_co2_savings) %>%
  arrange(risk_driver, desc(outcome_metric))

  
#Calculate cumulative numbers and tidy
cumulative <- tidy %>%
  group_by(outcome_metric) %>%
  summarise(baseline_2021 = sum(baseline_2021, na.rm = TRUE),
            no_adaptation_2030 = sum(no_adaptation_2030, na.rm = TRUE),
            adaptation_2030 = sum(adaptation_2030, na.rm = TRUE),
            no_adaptation_2050 = sum(no_adaptation_2050, na.rm = TRUE),
            adaptation_2050 = sum(adaptation_2050, na.rm = TRUE)) %>%
  as.data.frame() %>%
  ungroup() %>%
  mutate(risk_driver = "Cumulative") %>%
  arrange(desc(outcome_metric))
cumulative[cumulative == 0] <- NA
tidy <- rbind(tidy, cumulative) %>%
  mutate(outcome_metric = ifelse(outcome_metric == "Change in femissions (in MtCO2e) due to adaptation, direct", "Change in emissions (in MtCO2e) due to adaptation, direct", outcome_metric))

  
#Calculate change compared to baseline for deaths and DALYs
tidy2 <- tidy %>%
  mutate(change_no_adaptation_2030 = round((no_adaptation_2030/baseline_2021 - 1)*100, 1),
         change_adaptation_2030 = round((adaptation_2030/baseline_2021 - 1)*100, 1),
         change_no_adaptation_2050 = round((no_adaptation_2050/baseline_2021 - 1)*100, 1),
         change_adaptation_2050 = round((adaptation_2050/baseline_2021 - 1)*100, 1),
         baseline_2021 = round(baseline_2021, 2)) %>%
  mutate(no_adaptation_2030 = ifelse(outcome_metric == "Deaths (in millions)" | outcome_metric == "DALYs (in millions)", ifelse(change_no_adaptation_2030 > 0, paste0(round(no_adaptation_2030, 2), " (+", change_no_adaptation_2030, "%)"), paste0(round(no_adaptation_2030, 2), " (", change_no_adaptation_2030, "%)")), round(no_adaptation_2030, 2)),
         adaptation_2030 = ifelse(outcome_metric == "Deaths (in millions)" | outcome_metric == "DALYs (in millions)", ifelse(change_adaptation_2030 > 0, paste0(round(adaptation_2030, 2), " (+", change_adaptation_2030, "%)"), paste0(round(adaptation_2030, 2), " (", change_adaptation_2030, "%)")), round(adaptation_2030, 2)),
         no_adaptation_2050 = ifelse(outcome_metric == "Deaths (in millions)" | outcome_metric == "DALYs (in millions)", ifelse(change_no_adaptation_2050 > 0, paste0(round(no_adaptation_2050, 2), " (+", change_no_adaptation_2050, "%)"),paste0(round(no_adaptation_2050, 2), " (", change_no_adaptation_2050, "%)")), round(no_adaptation_2050, 2)),
         adaptation_2050 = ifelse(outcome_metric == "Deaths (in millions)" | outcome_metric == "DALYs (in millions)", ifelse(change_adaptation_2050 > 0, paste0(round(adaptation_2050, 2), " (+", change_adaptation_2050, "%)"), paste0(round(adaptation_2050, 2), " (", change_adaptation_2050, "%)")), round(adaptation_2050, 2))) %>%
  dplyr::select(c(risk_driver:adaptation_2050))


#Get alternative format of table
tidy3 <- tidy %>%
  mutate(adaptation_effect_2030 = no_adaptation_2030 - adaptation_2030,
         adaptation_effect_2050 = no_adaptation_2050 - adaptation_2050,
         adaptation_effect_2030 = ifelse(is.na(adaptation_effect_2030), adaptation_2030, adaptation_effect_2030),
         adaptation_effect_2050 = ifelse(is.na(adaptation_effect_2050), adaptation_2050, adaptation_effect_2050),
         outcome_metric = ifelse(outcome_metric == "Change in healthcare spending (in billion USD) due to adaptation", "Healthcare spending (in billion USD)",
                                 ifelse(outcome_metric == "Change in emissions (in MtCO2e) due to adaptation, direct", "Emissions, direct (in MtCO2e)",
                                        ifelse(outcome_metric == "Change in emissions (in MtCO2e) due to adaptation, indirect", "Emissions, indirect (in MtCO2e)", outcome_metric)))) %>%
  dplyr::select(c(risk_driver, outcome_metric, baseline_2021, no_adaptation_2030, no_adaptation_2050, adaptation_effect_2030, adaptation_effect_2050))


#Calculate change compared to baseline for deaths and DALYs
tidy4 <- tidy3 %>%
  mutate(change_no_adaptation_2030 = round((no_adaptation_2030/baseline_2021 - 1)*100, 1),
         change_no_adaptation_2050 = round((no_adaptation_2050/baseline_2021 - 1)*100, 1),
         change_adaptation_2030 = round((adaptation_effect_2030/no_adaptation_2030)*100, 1),
         change_adaptation_2050 = round((adaptation_effect_2050/no_adaptation_2050)*100, 1),
         baseline_2021 = round(baseline_2021, 2)) %>%
  mutate(no_adaptation_2030 = ifelse(outcome_metric == "Deaths (in millions)" | outcome_metric == "DALYs (in millions)", ifelse(change_no_adaptation_2030 > 0, paste0(round(no_adaptation_2030, 2), " (+", change_no_adaptation_2030, "%)"), paste0(round(no_adaptation_2030, 2), " (", change_no_adaptation_2030, "%)")), round(no_adaptation_2030, 2)),
         no_adaptation_2050 = ifelse(outcome_metric == "Deaths (in millions)" | outcome_metric == "DALYs (in millions)", ifelse(change_no_adaptation_2050 > 0, paste0(round(no_adaptation_2050, 2), " (+", change_no_adaptation_2050, "%)"),paste0(round(no_adaptation_2050, 2), " (", change_no_adaptation_2050, "%)")), round(no_adaptation_2050, 2)),
         adaptation_effect_2030 = ifelse(outcome_metric == "Deaths (in millions)" | outcome_metric == "DALYs (in millions)", paste0("-", round(adaptation_effect_2030, 2), " (-", change_adaptation_2030, "%)"), round(adaptation_effect_2030, 2)),
         adaptation_effect_2050 = ifelse(outcome_metric == "Deaths (in millions)" | outcome_metric == "DALYs (in millions)", paste0("-", round(adaptation_effect_2050, 2), " (-", change_adaptation_2050, "%)"), round(adaptation_effect_2050, 2))) %>%
  dplyr::select(c(risk_driver:adaptation_effect_2050))


#Save global summary table
if(run_choice == "main_run"){
  saveRDS(tidy2, "01_data/03_final/supplementary/table_s2_modelled_adaptation.rds")
  saveRDS(tidy4, "01_data/03_final/table_1.rds")
  write.csv(tidy4, "01_data/03_final/table_1.csv")
} else {
  saveRDS(tidy2, paste0("01_data/03_final/supplementary/", run_choice, "_table_s2_modelled_adaptation.rds"))
}



#3.2: Generate heat maps of adaptation effects ---------------------------------

if(run_choice == "main_run"){
  
  #Tidy datasets
  heat_country <- heat_country %>%
    dplyr::select(wb_country_code, measure, pop_baseline = city_pop_2020, heat_baseline = count_baseline,
                  heat_no_adapt_2030 = count_45_2030, heat_adapt_2030 = count_45_2030_adapt,
                  heat_no_adapt_2050 = count_45_2050, heat_adapt_2050 = count_45_2050_adapt)
  
  aq_country <- aq_country %>%
    dplyr::select(wb_country_code, measure, aq_baseline = count_baseline,
                  aq_no_adapt_2030 = count_2030, aq_adapt_2030 = count_2030_adapt,
                  aq_no_adapt_2050 = count_2050, aq_adapt_2050 = count_2050_adapt)
  
  wash_country <- wash_country %>%
    group_by(wb_country_code, measure) %>%
    summarise(wash_baseline = sum(count_baseline),
              wash_no_adapt_2030 = sum(count_2030),
              wash_adapt_2030 = sum(count_2030_adapt),
              wash_no_adapt_2050 = sum(count_2050),
              wash_adapt_2050 = sum(count_2050_adapt)) %>%
    as.data.frame() %>%
    ungroup()
              
  lifestyle_country <- lifestyle_country %>%
    group_by(wb_country_code, measure) %>%
    summarise(lifestyle_baseline = sum(count_baseline),
              lifestyle_no_adapt_2030 = sum(count_2030),
              lifestyle_adapt_2030 = sum(count_2030_adapt),
              lifestyle_no_adapt_2050 = sum(count_2050),
              lifestyle_adapt_2050 = sum(count_2050_adapt)) %>%
    as.data.frame() %>%
    ungroup()
  
  
  #Combine datasets
  country <- heat_country %>%
    left_join(aq_country, by = c("wb_country_code", "measure")) %>%
    left_join(wash_country, by = c("wb_country_code", "measure")) %>%
    left_join(lifestyle_country, by = c("wb_country_code", "measure"))
  
  
  #Get aggregate figures
  country <- country %>%
    mutate(baseline = heat_baseline + aq_baseline + wash_baseline + lifestyle_baseline,
           no_adapt_2030 = heat_no_adapt_2030 + aq_no_adapt_2030 + wash_no_adapt_2030 + lifestyle_no_adapt_2030,
           adapt_2030 = heat_adapt_2030 + aq_adapt_2030 + wash_adapt_2030 + lifestyle_adapt_2030,
           no_adapt_2050 = heat_no_adapt_2050 + aq_no_adapt_2050 + wash_no_adapt_2050 + lifestyle_no_adapt_2050,
           adapt_2050 = heat_adapt_2050 + aq_adapt_2050 + wash_adapt_2050 + lifestyle_adapt_2050) %>%
    dplyr::select(wb_country_code, measure, baseline:adapt_2050, pop_baseline)
  
  
  #Get percentage change
  country <- country %>%
    mutate(change_no_adapt_2030 = no_adapt_2030/baseline - 1,
           change_adapt_2030 = adapt_2030/baseline - 1,
           change_no_adapt_2050 = no_adapt_2050/baseline - 1,
           change_adapt_2050 = adapt_2050/baseline - 1)
  
  
  #Get adapt vs no adapt numbers
  country <- country %>%
    mutate(adapt_vs_no_adapt_2030 = (adapt_2030 - no_adapt_2030)/no_adapt_2030,
           adapt_vs_no_adapt_2050 = (adapt_2050 - no_adapt_2050)/no_adapt_2030)
  
  
  #Get baseline incidence rate
  country <- country %>%
    mutate(baseline_incidence_per_1000 = baseline / pop_baseline * 1000) %>%
    dplyr::select(-c(pop_baseline))
  
  
  #Split into mortality vs morbidity
  deaths <- country %>% 
    filter(measure == "Deaths") %>%
    dplyr::select(-c(measure))
  
  
  #Tidy world map
  world_map <- world_map %>%
    tidyterra::select(c(wb_country_code = ISO_A3))
  
  
  #Link with data
  deaths_map <- world_map %>%
    tidyterra::left_join(deaths, by = c("wb_country_code"))
  
  
  #Save heatmaps and data
  writeVector(deaths_map, "01_data/03_final/maps_mortality.gpkg", overwrite = TRUE)
  maps_mortality <- as.data.frame(deaths_map, geom = "WKT")
  write.csv(maps_mortality, "01_data/03_final/maps_mortality.csv", row.names = FALSE)
}


#Display completion message
cat(green("\nGenerating figures complete."))
