################################################################################
#NAME:         0304_generate_figures_archetypes.R
#AUTOR:        Alessa Widmaier and co-authors
#DATE:         03/05/2026
#DESCRIPTION:  Generate tables and figures for the academic article by city
#              archetype
#RUNNING TIME: <1min
################################################################################


################################################################################
#SECTION 1: INITIATE
################################################################################

#Display starting message
cat(green("\n\nGenerating figures by archetype started."))


#Create output folders
dir.create("01_data/03_final", recursive = TRUE, showWarnings = FALSE)
dir.create("01_data/03_final/supplementary", recursive = TRUE, showWarnings = FALSE)


#Read in additional CO2 savings from adaptation measures directly
direct_co2_savings <- readRDS("01_data/02_interim/08_cobenefits/archetypes_global_direct_co2_savings.rds")


#Start loop
tidy_all_alt <- NULL
for(archetype in c("established_ageing", "maturing", "transitioning", "fast_growing")){

  
  

################################################################################
#SECTION 2: LOAD DATA
################################################################################

#2.1: Read in required data for summary table ----------------------------------

  #Global summary table
  global <- read.csv(paste0("01_data/03_final/supplementary/", archetype, "_table_s1_alternative.csv"))
  
  
  
  
################################################################################
#SECTION 3: GET FIGURES
################################################################################

#3.1: Generate global summary table --------------------------------------------
  
  #Tidy additional CO2 savings data
  direct_co2_savings_archetype <- direct_co2_savings %>%
    filter(city_archetype == archetype) %>%
    dplyr::select(-c(city_archetype))
  
  
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
  direct_co2_savings_archetype <- direct_co2_savings_archetype %>%
  mutate(baseline_2021 = NA, 
         no_adaptation_2030 = NA, 
         adaptation_2030 = direct_co2_savings, 
         no_adaptation_2050 = NA, 
         adaptation_2050 = direct_co2_savings,
         outcome_metric = "Change in femissions (in MtCO2e) due to adaptation, direct") %>%
  dplyr::select(-c(direct_co2_savings))
  tidy <- rbind(tidy, direct_co2_savings_archetype) %>%
    arrange(risk_driver, desc(outcome_metric))
    
    
  #Calculate cumulative numbers
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
  
    
  #Get alternative format of table
  tidy_alt <- tidy %>%
    mutate(adaptation_effect_2030 = no_adaptation_2030 - adaptation_2030,
           adaptation_effect_2050 = no_adaptation_2050 - adaptation_2050,
           adaptation_effect_2030 = ifelse(is.na(adaptation_effect_2030), adaptation_2030, adaptation_effect_2030),
           adaptation_effect_2050 = ifelse(is.na(adaptation_effect_2050), adaptation_2050, adaptation_effect_2050),
           outcome_metric = ifelse(outcome_metric == "Change in healthcare spending (in billion USD) due to adaptation", "Healthcare spending (in billion USD)",
                                   ifelse(outcome_metric == "Change in emissions (in MtCO2e) due to adaptation, direct", "Emissions, direct (in MtCO2e)",
                                          ifelse(outcome_metric == "Change in emissions (in MtCO2e) due to adaptation, indirect", "Emissions, indirect (in MtCO2e)", outcome_metric)))) %>%
    dplyr::select(c(risk_driver, outcome_metric, baseline_2021, no_adaptation_2030, no_adaptation_2050, adaptation_effect_2030, adaptation_effect_2050)) %>%
    mutate(change_no_adaptation_2030 = round((no_adaptation_2030/baseline_2021 - 1)*100, 1),
           change_no_adaptation_2050 = round((no_adaptation_2050/baseline_2021 - 1)*100, 1),
           change_adaptation_2030 = round((adaptation_effect_2030/no_adaptation_2030)*100, 1),
           change_adaptation_2050 = round((adaptation_effect_2050/no_adaptation_2050)*100, 1),
           baseline_2021 = round(baseline_2021, 2)) %>%
    mutate(no_adaptation_2030 = ifelse(outcome_metric == "Deaths (in millions)" | outcome_metric == "DALYs (in millions)", ifelse(change_no_adaptation_2030 > 0, paste0(round(no_adaptation_2030, 2), " (+", change_no_adaptation_2030, "%)"), paste0(round(no_adaptation_2030, 2), " (", change_no_adaptation_2030, "%)")), round(no_adaptation_2030, 2)),
           no_adaptation_2050 = ifelse(outcome_metric == "Deaths (in millions)" | outcome_metric == "DALYs (in millions)", ifelse(change_no_adaptation_2050 > 0, paste0(round(no_adaptation_2050, 2), " (+", change_no_adaptation_2050, "%)"),paste0(round(no_adaptation_2050, 2), " (", change_no_adaptation_2050, "%)")), round(no_adaptation_2050, 2)),
           adaptation_effect_2030 = ifelse(outcome_metric == "Deaths (in millions)" | outcome_metric == "DALYs (in millions)", paste0("-", round(adaptation_effect_2030, 2), " (-", change_adaptation_2030, "%)"), round(adaptation_effect_2030, 2)),
           adaptation_effect_2050 = ifelse(outcome_metric == "Deaths (in millions)" | outcome_metric == "DALYs (in millions)", paste0("-", round(adaptation_effect_2050, 2), " (-", change_adaptation_2050, "%)"), round(adaptation_effect_2050, 2))) %>%
    mutate(city_archetype = archetype) %>%
    dplyr::select(c(city_archetype, risk_driver:adaptation_effect_2050))
  
  
  #Append and close loop
  tidy_all_alt <- rbind(tidy_all_alt, tidy_alt)
}




################################################################################
#SECTION 4: SAVE AND CLOSE
################################################################################

#4.1: Save ---------------------------------------------------------------------

#Save global summary table
saveRDS(tidy_all_alt, "01_data/03_final/table_s1.rds")
write.csv(tidy_all_alt, "01_data/03_final/table_s1.csv")

#Remove dataframes no longer needed
rm(global, tidy)

#Display completion message
cat(green("\nGenerating archetype figures complete."))
