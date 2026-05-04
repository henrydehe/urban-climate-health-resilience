################################################################################
#NAME:         0306_sensitivity_analysis.R
#AUTOR:        Alessa Widmaier and co-authors
#DATE:         03/05/2026
#DESCRIPTION:  Run sensitivity analysis scenarios and aggregate sensitivity
#              outputs
#RUNNING TIME: ~10min
################################################################################


################################################################################
#SECTION 1: INITIATE
################################################################################

#Display starting message
cat(green("\n\nSensitivity analysis started."))


#Create output folders
dir.create("01_data/03_final", recursive = TRUE, showWarnings = FALSE)


#Start loop across sensitivity scenarios
for(run_choice in c("adapt_low", "adapt_high")){


  
  
################################################################################
#SECTION 2: DEFINE SENSITIVITY PARAMETERS
################################################################################

#2.1: Adaptation intensity choices - low adaptation scenario -------------------

  if(run_choice == "adapt_low"){
    
    #Heat adaptation bundle
    tree_cover_increase <- 0.05/2                                                 #Share of city area
    green_roof_increase <- 0.05/2                                                 #Share of city area
    green_roof_buffer <- 2                                                        #Multiplier (i.e. 2 means that for every 100m2 of green roof, 200m2 are cooled)
    
    
    #Air quality adaptation bundle
    air_quality_urban_incidence <- "conservative"                                   #Choose "conservative" or "concentrated"
    air_quality_adapt <- readxl::read_excel("01_data/01_raw/04_air_quality/aq_interventions_evidence.xlsx", sheet = "aq_adaptation_impacts")
    air_quality_adapt_impact <- as.numeric(mean(air_quality_adapt$pm25_impact, na.rm = TRUE))/2
    air_quality_adapt_co2_impact <- as.numeric(mean(air_quality_adapt$co2_impact, na.rm = TRUE))/2
    
    
    #Lifestyle adaptation bundle
    pa_increase <- 108.5/2                                                        #Increase in physical activity in METs
    bmi_decrease_2030 <- 0.263/2                                                  #Decrease in BMI for people with BMI > 25 by 2030
    bmi_decrease_2040 <- 0.403/2                                                  #Decrease in BMI for people with BMI > 25 by 2040
    bmi_decrease_2050 <- 0.515/2                                                  #Decrease in BMI for people with BMI > 25 by 2050
    
    
    #WASH bundle
    wash_improve_community <- 0.1/2                                               #Share of people changing from "unimproved" to "improved" water/sewer access due to community measures
    wash_improve_infra <- 0.1/2                                                   #Share of people changing from "improved" to "piped" or "sewer" due to infrastructure investments
    wash_improve_handwash <- 0.05/2 
    
    
  
#2.2: Adaptation intensity choices - high adaptation scenario ------------------
  
  } else if(run_choice == "adapt_high"){
    
    #Heat adaptation bundle
    tree_cover_increase <- 0.05*2                                                 #Share of city area
    green_roof_increase <- 0.05*2                                                 #Share of city area
    green_roof_buffer <- 2                                                        #Multiplier (i.e. 2 means that for every 100m2 of green roof, 200m2 are cooled)
    
    
    #Air quality adaptation bundle
    air_quality_urban_incidence <- "conservative"                                   #Choose "conservative" or "concentrated"
    air_quality_adapt <- readxl::read_excel("01_data/01_raw/04_air_quality/aq_interventions_evidence.xlsx", sheet = "aq_adaptation_impacts")
    air_quality_adapt_impact <- as.numeric(mean(air_quality_adapt$pm25_impact, na.rm = TRUE))*2
    air_quality_adapt_co2_impact <- as.numeric(mean(air_quality_adapt$co2_impact, na.rm = TRUE))*2
    
    
    #Lifestyle adaptation bundle
    pa_increase <- 108.5*2                                                        #Increase in physical activity in METs
    bmi_decrease_2030 <- 0.263*2                                                  #Decrease in BMI for people with BMI > 25 by 2030
    bmi_decrease_2040 <- 0.403*2                                                  #Decrease in BMI for people with BMI > 25 by 2040
    bmi_decrease_2050 <- 0.515*2                                                  #Decrease in BMI for people with BMI > 25 by 2050
    
    
    #WASH bundle
    wash_improve_community <- 0.1*2                                               #Share of people changing from "unimproved" to "improved" water/sewer access due to community measures
    wash_improve_infra <- 0.1*2                                                   #Share of people changing from "improved" to "piped" or "sewer" due to infrastructure investments
    wash_improve_handwash <- 0.05*2 
    
  }

  
  

################################################################################
#SECTION 3: RUN SCRIPTS
################################################################################

#3.1: Model --------------------------------------------------------------------

  #Run scripts for each driver
  source("02_codes/0201_model_air_quality.R")
  source("02_codes/0202_model_temp.R")
  source("02_codes/0203_model_wash.R")
  source("02_codes/0204_model_lifestyle.R")
  source("02_codes/0205_model_cobenefits.R")


  
#3.2: Post-model analysis ------------------------------------------------------

  #Run scripts to get results
  source("02_codes/0301_summary_results.R")
  source("02_codes/0303_generate_figures.R")
  
  
#Close loop across sensitivity scenarios
}




################################################################################
#SECTION 4: GENERATE SENSITIVITY ANALYSIS TABLE
################################################################################

#Read in results tables
main <- readRDS("01_data/03_final/supplementary/table_s2_modelled_adaptation.rds") %>% mutate(sensitivity = "ZModelled adaptation", row_number = row_number())
low <- readRDS("01_data/03_final/supplementary/adapt_low_table_s2_modelled_adaptation.rds") %>% mutate(sensitivity = "Low adaptation", row_number = row_number())
high <- readRDS("01_data/03_final/supplementary/adapt_high_table_s2_modelled_adaptation.rds") %>% mutate(sensitivity = "High adaptation", row_number = row_number())


#Combine
results <- rbind(main, low, high)
results <- results %>% 
  arrange(row_number, desc(sensitivity)) %>%
  mutate(sensitivity = ifelse(sensitivity == "ZModelled adaptation", "Modelled adaptation", sensitivity)) %>%
  dplyr::select(c(risk_driver, outcome_metric, sensitivity, adaptation_2030, adaptation_2050))


#Save
saveRDS(results, "01_data/03_final/table_s2.rds")
write.csv(results, "01_data/03_final/table_s2.csv")


#Display completion message
cat(green("\nRunning sensitivity analysis complete."))
