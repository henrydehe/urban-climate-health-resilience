################################################################################
#NAME:         00_full_run.R
#AUTOR:        Alessa Widmaier and co-authors
#DATE:         03/05/2026
#DESCRIPTION:  Run full urban climate-health resilience workflow
#RUNNING TIME: TBD
################################################################################


################################################################################
#SECTION 1: INITIATE
################################################################################


#1.1: Prepare for analysis -----------------------------------------------------

#Clear environment
cat("\014")
rm(list= ls())
gc()


#1.2: Packages -----------------------------------------------------------------

#List required/useful packages
packages <- c("plyr", "dplyr", "cli", "tidyr", "httr", "ggplot2", "readxl", "tibble",
              "writexl", "openxlsx", "stringr", "readr", "tidyverse", "haven", 
              "labelled", "terra", "crayon", "zoo", "caTools", "logr", "sandwich", 
              "plm", "sf", "spdep", "rmarkdown", "raster", "data.table", "lubridate",
              "exactextractr", "fasterize", "agrmt", "rasterVis", "sp", "tictoc", 
              "tidyterra", "beepr", "ncdf4", "CFtime", "lattice", "RColorBrewer", 
              "remotes", "forecast", "purrr", "countrycode", "truncnorm", "nleqslv")


#Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}


#Load packages
suppressMessages(suppressWarnings(invisible(lapply(packages, library, character.only=TRUE))))


#Set global options
options(scipen = 999)
options(dplyr.summarise.inform = FALSE)
options(readr.show_col_types = FALSE)
options(warn = 0)
source("02_codes/0306_sensitivity_analysis.R")


#Display starting message
cat(green("\n\nFull model run started."))




################################################################################
#SECTION 2: MODEL RUN CHOICES
################################################################################

#2.1: High-level choices -------------------------------------------------------

#Choose coordinate reference system             
crs_choice <- "EPSG:4326"


#Choose years and scenarios
year_choice <- c(2020, 2030, 2040, 2050)                                        #Years of analysis (include baseline and future)
scenario_choice <- c(45, 85)                                                    #RCP scenarios


#Choose baseline year
year_baseline <- 2020                                                           #Must be subset of year_choice


#Climate-sensitive threshold for lifestyle and WASH
climate_sens_thresh <- 0                                                        #Threshold above which a condition counts as climate-sensitive (e.g. if more than 5% of heat-related deaths/DALYs are due to stroke, stroke is a climate-sensitive condition)


#2.2: Adaptation intensity choices ---------------------------------------------

#Set to main run
run_choice <- "main_run"                                              


#Heat adaptation bundle
tree_cover_increase <- 0.05                                                     #Share of city area
green_roof_increase <- 0.05                                                     #Share of city area
green_roof_buffer <- 2                                                          #Multiplier (i.e. 2 means that for every 100m2 of green roof, 200m2 are cooled)


#Air quality adaptation bundle
air_quality_urban_incidence <- "conservative"                                   #Choose "conservative" or "concentrated"
air_quality_adapt <- readxl::read_excel("01_data/01_raw/04_air_quality/aq_interventions_evidence.xlsx", sheet = "aq_adaptation_impacts")
air_quality_adapt_impact <- as.numeric(mean(air_quality_adapt$pm25_impact, na.rm = TRUE))
air_quality_adapt_co2_impact <- as.numeric(mean(air_quality_adapt$co2_impact, na.rm = TRUE))


#Lifestyle adaptation bundle
pa_increase <- 108.5                                                            #Increase in physical activity in METs
bmi_decrease_2030 <- 0.263                                                      #Decrease in BMI for people with BMI > 25 by 2030
bmi_decrease_2040 <- 0.403                                                      #Decrease in BMI for people with BMI > 25 by 2040
bmi_decrease_2050 <- 0.515                                                      #Decrease in BMI for people with BMI > 25 by 2050


#WASH bundle
wash_improve_community <- 0.1                                                   #Share of people changing from "unimproved" to "improved" water/sewer access due to community measures
wash_improve_infra <- 0.1                                                       #Share of people changing from "improved" to "piped" or "sewer" due to infrastructure investments
wash_improve_handwash <- 0.05                                                   #Share of people gaining access to handwashing facilities (if they don't have access already)


################################################################################
#SECTION 3: ADAPTATION MODELLING                   
################################################################################

#3.1: Prepare data -------------------------------------------------------------
tic()
#Clean disease data
source("02_codes/0101_clean_disease.R")


#Clean city data
source("02_codes/0102_clean_cities.R")


#Clean climate projections
source("02_codes/0103_clean_climate.R")


#Clean urban greening and green roof data
source("02_codes/0104_clean_urban_greening.R")
source("02_codes/0105_clean_green_roofs.R")


#Clean healthcare spending and emissions data
source("02_codes/0106_clean_spending_emissions.R")


#Clean exposure projections by country and risk driver
source("02_codes/0107_clean_projections.R")


#Clean urban-rural split
source("02_codes/0108_clean_urban_rural.R")


#3.2: Modelling by driver ------------------------------------------------------

#Model air-quality-related health effects
source("02_codes/0201_model_air_quality.R")


#Model heat-related health effects
source("02_codes/0202_model_temp.R")


#Model WASH effects
source("02_codes/0203_model_wash.R")


#Model lifestyle effects
source("02_codes/0204_model_lifestyle.R")


#Model intervention co-benefits (CO2 reductions, revenues)
source("02_codes/0205_model_cobenefits.R")


#3.3: Post-model analysis ------------------------------------------------------

#Generate summary results
source("02_codes/0301_summary_results.R")
source("02_codes/0302_summary_results_archetypes.R")


#Generate figures for article
source("02_codes/0303_generate_figures.R")
source("02_codes/0304_generate_figures_archetypes.R")


#Generate vulnerability scores
source("02_codes/0305_generate_vulnerability_scores.R")


#Conduct sensitivity analysis
source("02_codes/0306_sensitivity_analysis.R")


#Generate figures used in-text in the article
source("02_codes/0307_generate_figures_for_text.R")




################################################################################
#SECTION 4: CLOSE                  
################################################################################
toc()
#Display completion message
cat(green("\nFull model run complete."))
