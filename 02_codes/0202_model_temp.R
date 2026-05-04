################################################################################
#NAME:         0202_model_temp.R
#AUTOR:        Alessa Widmaier and co-authors
#DATE:         03/05/2026
#DESCRIPTION:  Model heat-related mortality/morbidity and adaptation impacts of
#              the urban heat-reducing bundle
#RUNNING TIME: ~2min
################################################################################


################################################################################
#SECTION 1: INITIATE
################################################################################

#Display starting message
cat(green("\n\nModelling heat started."))




################################################################################
#SECTION 2: LOAD DATA
################################################################################

#2.1: Read in required data ----------------------------------------------------

#City-level climate data
climate <- readxl::read_excel("01_data/01_raw/01_cities/GHSL_Cities_DB/GHS_UCDB_GLOBE_R2024A.xlsx", sheet = "CLIMATE")
temp_proj <- readRDS("01_data/02_interim/05_heat/temperature_projections.rds")


#Baseline deaths & DALYs at city level
bl_health <- readRDS("01_data/02_interim/02_health_baseline/city_level_disease.rds")


#Heat RR data
heat_rr <- readxl::read_excel("01_data/01_raw/05_heat/heat_RR_curves.xlsx", sheet = "extracted_data_adj", skip = 1)


#Adaptation impacts
tree_cover <- terra::vect("01_data/02_interim/01_cities/cities.gpkg") %>% as.data.frame()
trees <- read.csv("01_data/02_interim/05_heat/ghsl_tree_cover_cooling_effect.csv")
roofs <- read.csv("01_data/02_interim/05_heat/ghsl_green_roof_cooling_effect.csv")


#Population growth
pop_growth <- readRDS("01_data/02_interim/01_cities/population_growth.rds")


#Urban-rural split
urb_rur <- readRDS("01_data/02_interim/05_heat/heat_urban_rural.rds")




################################################################################
#SECTION 3: CLEAN DATA
################################################################################

#3.1: Clean temperature data ---------------------------------------------------

#Get data until including 2030 from GHSL dataset
for(sc in scenario_choice){
  for(yr in year_choice){
    if(yr <= 2030){
      
      
      #Select required data
      temp_sc_yr <- climate %>%
        rename(mean_temp = paste0("CL_B01_P", sc, "_", yr),
               sd_temp = paste0("CL_B04_P", sc, "_", yr)) %>%
        dplyr::select(city_id = ID_UC_G0, city_name = GC_UCN_MAI_2025, mean_temp, sd_temp) %>%
        mutate(sd_temp = sd_temp / 100)
      
      
      #Assign name
      assign(paste0("temp_", sc, "_", yr), temp_sc_yr)
      
      
      #Tidy and close
      rm(temp_sc_yr)
    }
  }
}


#Get data after 2030 from Copernicus spatial maps
for(sc in scenario_choice){
  for(yr in year_choice){
    if(yr > 2030){
      
      
      #Get 2030 data from GHSL in
      temp_sc_yr <- get(paste0("temp_", sc, "_2030"))
      
      
      #Get absolute temperature change compared to 2030 from Copernicus
      temp_change_sc_yr <- temp_proj %>%
        rename(temp_2030 = paste0("mean_temp_", sc, "_2030"),
               temp_future = paste0("mean_temp_", sc, "_", yr)) %>%
        mutate(temp_change = temp_future - temp_2030,
               temp_change = ifelse(is.na(temp_change), 0, temp_change)) %>%
        dplyr::select(c(city_id, city_name, temp_change))
      
      
      #Bring together and adjust mean temperature (sd stays the same as in 2030)
      temp_sc_yr <- temp_sc_yr %>%
        left_join(temp_change_sc_yr, by = c("city_id", "city_name")) %>%
        mutate(mean_temp = ifelse(is.na(temp_change), mean_temp, mean_temp + temp_change)) %>%
        dplyr::select(-c(temp_change))
      
      
      #Assign name
      assign(paste0("temp_", sc, "_", yr), temp_sc_yr)
      
      
      #Tidy and close
      rm(temp_sc_yr, temp_change_sc_yr)
    }
  }
}



#3.2: Clean heat RR data -------------------------------------------------------

#Tidy and pivot
heat_rr <- heat_rr[complete.cases(heat_rr),]
heat_rr <- heat_rr %>%
  mutate_all(as.numeric) %>%
  pivot_longer(2:ncol(heat_rr), names_to = "mean_annual_temp", values_to = "log_rr") %>%
  dplyr::select(c(mean_annual_temp, mean_daily_temp = 1, log_rr)) %>%
  mutate(mean_annual_temp = as.numeric(mean_annual_temp)) %>%
  arrange(mean_annual_temp, mean_daily_temp)


#Convert from log RR to RR
heat_rr <- heat_rr %>%
  mutate(heat_rr = exp(log_rr) - 1) %>%
  dplyr::select(-c(log_rr))



#3.3: Clean baseline health data -----------------------------------------------

#Keep only relevant variables and tidy
bl_health <- bl_health %>%
  dplyr::select(c(city_id = unique_id, city_name = urban_centre_main_name, city_share = city_pct_country_pop,
                  measure, rei, count_baseline = city_number_conservative)) %>%
  filter(rei == "High temperature") %>%
  group_by(city_id, city_name, city_share, measure) %>%
  summarise(count_baseline = sum(count_baseline, na.rm = TRUE)) %>%
  as.data.frame() %>%
  ungroup()



#3.4: Clean adaptation impacts - tree cover ------------------------------------

#Tidy tree cover impact
trees <- trees %>%
  dplyr::select(c(city_id = unique_id, city_name = urban_centre_main_name, tce_10_20:tce_30_30)) %>%
  pivot_longer(cols = c(tce_10_20:tce_30_30), names_to = "variable", values_to = "temp_reduction") %>%
  mutate(tree_cover = as.numeric(str_sub(variable, 5, 6)),
         air_temp = as.numeric(str_sub(variable, 8, 9))) %>%
  dplyr::select(c(city_id, city_name, air_temp, tree_cover, temp_reduction)) %>%
  arrange(city_id, air_temp, tree_cover)


#Get starting and finish value of tree cover increase
tree_cover1 <- tree_cover %>%
  mutate(trees_share = ifelse(is.nan(trees_share), 10, floor(100 * trees_share))) %>%
  dplyr::select(c(city_id, city_name, tree_cover = trees_share))
tree_cover2 <- tree_cover1 %>%
  mutate(tree_cover = tree_cover + tree_cover_increase * 100 - 1)


#Interpolate between values
tree_cover <- tree_cover1 %>%
  rbind(tree_cover2) %>%
  group_by(city_id, city_name) %>%
  arrange(city_id, tree_cover) %>%
  complete(tree_cover = seq(min(tree_cover), max(tree_cover), by = 1)) %>%
  ungroup() 
  

#Interpolate values for missing tree cover values
trees <- trees %>%
  group_by(city_id, city_name, air_temp) %>%
  arrange(tree_cover, .by_group = TRUE) %>%
  complete(tree_cover = seq(0, 100, by = 1)) %>%
  mutate(temp_reduction = zoo::na.approx(temp_reduction, tree_cover, rule = 2)) %>%
  ungroup()

  
#Get total temperature reduction across range of tree cover (from starting to ending tree cover)
tree_cover <- tree_cover %>%
  left_join(trees, by = c("city_id", "city_name", "tree_cover")) %>%
  group_by(city_id, city_name, air_temp) %>%
  summarise(temp_reduction_trees = sum(temp_reduction, na.rm = TRUE)) %>%
  as.data.frame() %>%
  ungroup() %>%
  dplyr::select(c(city_id, city_name, air_temp, temp_reduction_trees))



#3.5: Clean adaptation impacts - cool roofs ------------------------------------

#Tidy roof impact
roofs <- roofs %>%
  dplyr::select(c(city_id = unique_id, city_name = urban_centre_main_name, temp_reduction_roofs = change_in_rooftop_temp)) %>%
  mutate(temp_reduction_roofs = temp_reduction_roofs * green_roof_increase * green_roof_buffer,
         temp_reduction_roofs = ifelse(is.na(temp_reduction_roofs), 0, temp_reduction_roofs))


#Merge
adapt_impact <- tree_cover %>%
  left_join(roofs, by = c("city_id", "city_name")) %>%
  mutate(temp_reduction = temp_reduction_trees + temp_reduction_roofs,
         temp_reduction = 0.5 * floor(temp_reduction/0.5),
         temp_reduction = ifelse(temp_reduction < 0, 0, temp_reduction)) %>%
  dplyr::select(-c(temp_reduction_trees, temp_reduction_roofs))
rm(trees, roofs)



#3.6: Calculate baseline risk rate ---------------------------------------------

#Baseline risk ratio (death or DALY cases per person)
rr_bl <- bl_health %>%
  left_join(pop_growth, by = c("city_id", "city_name")) %>%
  mutate(rr_baseline = count_baseline/population) %>%
  filter(measure == "Deaths") %>%
  dplyr::select(c(city_id, city_name, rr_baseline))
  



################################################################################
#SECTION 4: MODEL CITY-LEVEL TEMPERATURE DISTRIBUTIONS
################################################################################

#4.1: Define function to calculate temperature distributions -------------------

#Function to extract number of days in each 0.5C temperature bucket (truncated normal distribution)
compute_buckets <- function(city_identifier, city_name_long, mean_temperature, sd_temperature) {
  min_temperature <- mean_temperature - 4 * sd_temperature
  max_temperature <- mean_temperature + 4 * sd_temperature
  bucket_min <- floor(min_temperature * 2) / 2
  bucket_max <- ceiling(max_temperature * 2) / 2
  breaks <- seq(bucket_min, bucket_max, by = 0.5)
  bucket_labels <- paste(head(breaks, -1))
  breaks_clamped <- pmin(pmax(breaks, min_temperature), max_temperature)
  probs <- ptruncnorm(tail(breaks_clamped, -1), a = min_temperature, b = max_temperature, mean = mean_temperature, sd = sd_temperature) -
           ptruncnorm(head(breaks_clamped, -1), a = min_temperature, b = max_temperature, mean = mean_temperature, sd = sd_temperature)
  data.frame(
    city_id = city_identifier,
    city_name = city_name_long,
    temp_bucket = bucket_labels,
    share_days = probs
  )
}



#4.2: Get temperature distributions for baseline and future --------------------

#Get expected number of days in each 0.5C bucket for all scenario/year combinations
for(sc in scenario_choice){
  count_yr <- 0
  for(yr in year_choice){
    
    
    #Get data
    temp_sc_yr <- get(paste0("temp_", sc, "_", yr))
    
    
    #Apply function row-wise to each scenario/year dataset and bind results
    temp_sc_yr <- temp_sc_yr %>%
      rowwise() %>%
      mutate(bucket_data = list(compute_buckets(city_id, city_name, mean_temp, sd_temp))) %>%
      select(bucket_data) %>%
      unnest(cols = c(bucket_data))
    colnames(temp_sc_yr) <- c("city_id", "city_name", "temp_bucket", paste0("share_days_", sc, "_", yr))
    
    
    #Filter out any observations below minimum temperature in heat_rr dataset
    temp_sc_yr <- temp_sc_yr %>%
      mutate(temp_bucket = as.numeric(temp_bucket)) %>%
      filter(temp_bucket >= min(heat_rr$mean_daily_temp))
    
  
    #Get joint file for each scenario
    if(count_yr == 0){
      temp_sc <- temp_sc_yr
    } else {
      temp_sc <- temp_sc %>%
        full_join(temp_sc_yr, by = c("city_id", "city_name", "temp_bucket"))
    }
    
    
    #Tidy and close loop across years
    rm(temp_sc_yr)
    count_yr <- count_yr + 1
  }
  
  
  #Tidy 
  temp_sc <- temp_sc %>%
    mutate(temp_bucket = as.numeric(temp_bucket))
  temp_sc[is.na(temp_sc)] <- 0
    
  
  #Bring in baseline mean annual temperature
  temp_sc_baseline <- get(paste0("temp_", sc, "_", year_baseline)) %>% 
    dplyr::select(-c(sd_temp))
  temp_sc <- temp_sc %>%
    left_join(temp_sc_baseline, by = c("city_id", "city_name")) %>%
    relocate(city_id, city_name, mean_temp_baseline = mean_temp)
  
  
  #Assign name
  assign(paste0("temp_", sc), temp_sc)
  rm(temp_sc, count_yr)
}




################################################################################
#SECTION 5: CALCULATE CITY-LEVEL TEMPERATURE DISTRIBUTIONS WITH ADAPTATION
################################################################################

#5.1: Reduce temperatures based on adaptation impacts --------------------------

for(sc in scenario_choice){
  
  #Get data
  temp_sc <- get(paste0("temp_", sc))
  
  
  #Shift distributions downwards  
  temp_sc_adapt <- temp_sc %>%
    mutate(air_temp = ifelse(temp_bucket >= 30, 30,
                             ifelse(temp_bucket >= 25, 25, 
                                    ifelse(temp_bucket >= 20, 20, NA)))) %>%
    left_join(adapt_impact, by = c("city_id", "city_name", "air_temp")) %>%
    mutate(temp_bucket = ifelse(is.na(temp_reduction), temp_bucket, temp_bucket - temp_reduction)) %>%
    dplyr::select(-c(air_temp, temp_reduction))
  
  
  #Tidy
  colnames(temp_sc_adapt) <- c("city_id", "city_name", "mean_temp_baseline", "temp_bucket", paste0("share_days_", sc, "_", c(year_choice), "_adapt"))
  temp_sc_adapt <- temp_sc_adapt %>%
    dplyr::select(-c(5))
    
  
  #Join together
  temp_sc <- temp_sc %>%
    full_join(temp_sc_adapt, by = c("city_id", "city_name", "mean_temp_baseline", "temp_bucket")) %>% 
    filter(temp_bucket >= min(heat_rr$mean_daily_temp)) %>%
    replace(is.na(.), 0)
  
  
  #Assign name
  assign(paste0("temp_", sc), temp_sc)
  rm(temp_sc)
}
  
  


################################################################################
#SECTION 6: CALCULATE HEAT-RELATED RELATIVE RISKS
################################################################################
  
#6.1: Bring in climate-zone specific relative risk -----------------------------

#Bring in log RR figures for each bucket
count <- 0
for(sc in scenario_choice){


  #Get data
  temp_sc <- get(paste0("temp_", sc))
  
  
  #Tidy mean annual temperature metric
  temp_sc <- temp_sc %>%
    mutate(mean_annual_temp = round(mean_temp_baseline),
           mean_annual_temp = ifelse(mean_annual_temp < min(heat_rr$mean_annual_temp), min(heat_rr$mean_annual_temp),
                                     ifelse(mean_annual_temp > max(heat_rr$mean_annual_temp), max(heat_rr$mean_annual_temp), mean_annual_temp))) %>%
    dplyr::select(-c(mean_temp_baseline)) %>%
    relocate(city_id, city_name, mean_annual_temp, temp_bucket)
  
  
  #Exclude days below minimum daily temp range in heat_rr dataset
  temp_sc <- temp_sc %>%
    filter(temp_bucket >= min(heat_rr$mean_daily_temp))
  
  
  #Sum all days with temperatures above daily temperature range in heat_rr dataset
  temp_sc <- temp_sc %>%
    mutate(temp_bucket = ifelse(temp_bucket > max(heat_rr$mean_daily_temp), max(heat_rr$mean_daily_temp), temp_bucket)) %>%
    group_by(city_id, city_name, temp_bucket) %>%
    mutate_at(vars(5:ncol(temp_sc)), sum) %>%
    ungroup() %>%
    distinct()
  
  
  #Left-join based on mean annual temperature and mean daily temperature
  temp_sc <- temp_sc %>%
    left_join(heat_rr, by = c("mean_annual_temp", "temp_bucket" = "mean_daily_temp")) %>%
    dplyr::select(-c(mean_annual_temp, temp_bucket)) %>%
    relocate(city_id, city_name, heat_rr)
  
  
  
#6.2: Calculate city-specific change in relative risk --------------------------
  
  #Calculate weighted average relative risk at city-level
  temp_sc <- temp_sc %>%
    mutate(across(c(4:ncol(temp_sc)), ~ .x * heat_rr)) %>%
    group_by(city_id, city_name) %>%
    mutate_at(vars(4:ncol(temp_sc)), sum) %>%
    ungroup() %>%
    dplyr::select(-c(heat_rr)) %>%
    distinct()
  
  
  #Tidy
  colnames(temp_sc) <- c("city_id", "city_name", paste0("heat_rr_", sc, "_", c(year_choice)), paste0("heat_rr_", sc, "_", c(year_choice[-1]), "_adapt"))
  
  
  #Calculate change compared to baseline
  count_yr <- 0
  for(yr in year_choice){
    if(yr != year_baseline) {
      temp_sc_change_yr <- temp_sc %>%
        left_join(rr_bl, by = c("city_id", "city_name")) %>%
        rename(heat_rr_baseline = paste0("heat_rr_", sc, "_", year_baseline),
               heat_rr_future = paste0("heat_rr_", sc, "_", yr),
               heat_rr_future_adapt = paste0("heat_rr_", sc, "_", yr, "_adapt"))
      
      
      #Relative change (% change compared to baseline)
      temp_sc_change_yr <- temp_sc_change_yr %>%
        mutate(heat_rr_change = ifelse(heat_rr_baseline == 0, 0, pmax(heat_rr_future / heat_rr_baseline - 1, 0)),
               heat_rr_change_adapt = ifelse(heat_rr_baseline == 0, 0, heat_rr_future_adapt / heat_rr_baseline - 1),
               heat_rr_change_adapt = pmin(heat_rr_change_adapt, heat_rr_change)) %>%

        
      #Tidy
        dplyr::select(c(city_id, city_name, heat_rr_change, heat_rr_change_adapt))
      colnames(temp_sc_change_yr) <- c("city_id", "city_name", paste0("count_", sc, "_", yr), paste0("count_", sc, "_", yr, "_adapt"))
      
      
      #Append
      if(count_yr == 0){
        temp_sc_change <- temp_sc_change_yr
      } else {
        temp_sc_change <- temp_sc_change %>% left_join(temp_sc_change_yr, by = c("city_id", "city_name"))
      }
      count_yr <- count_yr + 1
      rm(temp_sc_change_yr)
    }
  }
  
  
  #Merge across scenarios
  if(count == 0){
    temp <- temp_sc_change
  } else {
    temp <- temp %>% left_join(temp_sc_change, by = c("city_id", "city_name"))
  }
  
  
  #Tidy and close
  rm(temp_sc)
  count <- count + 1
}
rm(count)




################################################################################
#SECTION 7: ESTIMATE DEATHS AND DALYs
################################################################################

#7.1: Adjust baseline numbers for urban/rural split ----------------------------

#Get country-level and city-level baseline temperature distributions
urb_rur_country <- urb_rur %>%
  dplyr::select(c(wb_country_code, temp_country, temp_sd_country)) %>%
  distinct() 
temp_country <- urb_rur_country %>%
  rowwise() %>%
  mutate(bucket_data = list(compute_buckets(wb_country_code, wb_country_code, temp_country, temp_sd_country))) %>%
  select(bucket_data) %>%
  unnest(cols = c(bucket_data)) %>%
  mutate(temp_bucket = as.numeric(temp_bucket)) %>%
  filter(temp_bucket >= min(heat_rr$mean_daily_temp)) %>%
  dplyr::select(c(wb_country_code = 1, temp_bucket, share_days))
temp_city <- urb_rur %>%
  rowwise() %>%
  mutate(bucket_data = list(compute_buckets(city_id, city_name, temp_city, temp_sd_city))) %>%
  select(bucket_data) %>%
  unnest(cols = c(bucket_data)) %>%
  mutate(temp_bucket = as.numeric(temp_bucket)) %>%
  filter(temp_bucket >= min(heat_rr$mean_daily_temp))


#Bring in baseline mean annual temperature
temp_country <- temp_country %>%
  left_join(urb_rur_country, by = c("wb_country_code")) %>%
  mutate(mean_annual_temp = round(temp_country)) %>%
  dplyr::select(c(wb_country_code, mean_annual_temp, temp_bucket, share_days))
temp_city <- temp_city %>%
  left_join(urb_rur, by = c("city_id", "city_name")) %>%
  mutate(mean_annual_temp = round(temp_country)) %>%
  dplyr::select(c(city_id, city_name, wb_country_code, mean_annual_temp, temp_bucket, share_days))


#Compute baseline RR at country- and city-level
temp_country <- temp_country %>%
  filter(temp_bucket >= min(heat_rr$mean_daily_temp)) %>%
  mutate(mean_annual_temp = ifelse(mean_annual_temp < min(heat_rr$mean_annual_temp), min(heat_rr$mean_annual_temp),
                                   ifelse(mean_annual_temp > max(heat_rr$mean_annual_temp), max(heat_rr$mean_annual_temp), mean_annual_temp)),
         temp_bucket = ifelse(temp_bucket > max(heat_rr$mean_daily_temp), max(heat_rr$mean_daily_temp), temp_bucket)) %>%
  left_join(heat_rr, by = c("mean_annual_temp", "temp_bucket" = "mean_daily_temp")) %>%
  mutate(heat_rr = share_days * heat_rr) %>%
  group_by(wb_country_code) %>%
  summarise(heat_rr_country = sum(heat_rr)) %>%
  ungroup()
temp_city <- temp_city %>%
  filter(temp_bucket >= min(heat_rr$mean_daily_temp)) %>%
  mutate(mean_annual_temp = ifelse(mean_annual_temp < min(heat_rr$mean_annual_temp), min(heat_rr$mean_annual_temp),
                                   ifelse(mean_annual_temp > max(heat_rr$mean_annual_temp), max(heat_rr$mean_annual_temp), mean_annual_temp)),
         temp_bucket = ifelse(temp_bucket > max(heat_rr$mean_daily_temp), max(heat_rr$mean_daily_temp), temp_bucket)) %>%
  left_join(heat_rr, by = c("mean_annual_temp", "temp_bucket" = "mean_daily_temp")) %>%
  mutate(heat_rr = share_days * heat_rr) %>%
  group_by(city_id, city_name) %>%
  summarise(heat_rr_city = sum(heat_rr)) %>%
  ungroup()


#Bring together
urb_rur_adj <- urb_rur %>%
  dplyr::select(c(city_id, city_name, wb_country_code, pop_country, pop_city)) %>%
  left_join(temp_country, by = c("wb_country_code")) %>%
  left_join(temp_city, by = c("city_id", "city_name")) %>%
  mutate(rate_wrt_country = heat_rr_city/heat_rr_country,
         rate_wrt_country = ifelse(heat_rr_country == 0 & heat_rr_city == 0, 1, rate_wrt_country)) %>%
  dplyr::select(city_id, city_name, wb_country_code, rate_wrt_country)


#Adjust baseline health data
bl_health <- bl_health %>%
  mutate(count_baseline_country = count_baseline / city_share) %>%
  left_join(urb_rur_adj, by = c("city_id", "city_name")) %>%
  mutate(count_baseline_unadj = ifelse(is.na(rate_wrt_country), count_baseline, count_baseline * rate_wrt_country)) %>%
  group_by(wb_country_code, measure) %>%
  mutate(count_baseline_unadj = ifelse(is.infinite(count_baseline_unadj), count_baseline_country * city_share / sum(city_share), count_baseline_unadj),
         count_baseline_country_unadj = sum(count_baseline_unadj),
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
  

  
#7.2: Project deaths and DALYs into future -------------------------------------

#Bring together with baseline data
temp_death_daly <- bl_health %>%
  dplyr::select(c(city_id, city_name, measure, count_baseline = count_baseline_adj)) %>% 
  left_join(temp, by = c("city_id", "city_name"))


#Multiply deaths and DALYs by change in heat RR under different scenarios
temp_death_daly <- temp_death_daly %>%
  mutate(across(c(5:ncol(temp_death_daly)), ~ (.x + 1) * count_baseline))


#Harmonise RCP8.5 and RCP4.5 results
temp_death_daly <- temp_death_daly %>%
  mutate(count_45_2030 = pmin(count_45_2030, count_85_2030),
         count_45_2030_adapt = pmin(count_45_2030_adapt, count_85_2030_adapt),
         count_45_2040 = pmin(count_45_2040, count_85_2040),
         count_45_2040_adapt = pmin(count_45_2040_adapt, count_85_2040_adapt),
         count_45_2050 = pmin(count_45_2050, count_85_2050),
         count_45_2050_adapt = pmin(count_45_2050_adapt, count_85_2050_adapt),
         risk = "High temperature") %>%
  relocate(city_id, city_name, risk, measure) %>%
  arrange(city_id, risk, measure)
         

#Bring in population growth rates
temp_death_daly_pop <- temp_death_daly %>%
  left_join(pop_growth, by = c("city_id", "city_name"))


#Multiply by population growth
count <- 0
for(sc in scenario_choice){
  for(yr in year_choice){
    if(yr != year_baseline){
      temp_sc_yr <- temp_death_daly_pop %>%
        rename(count = paste0("count_", sc, "_", yr),
               count_adapt = paste0("count_", sc, "_", yr, "_adapt"),
               pop_multiplier = paste0("pop_multiplier_", yr)) %>%
        mutate(count_new = count * pop_multiplier,
               count_adapt_new = count_adapt * pop_multiplier) %>%
        dplyr::select(c(city_id, city_name, risk, measure, count_new, count_adapt_new))
      colnames(temp_sc_yr) <- c("city_id", "city_name", "risk", "measure", paste0("count_", sc, "_", yr), paste0("count_", sc, "_", yr, "_adapt"))
      if(count == 0){
        temp <- temp_sc_yr
      } else {
        temp <- temp %>%
          left_join(temp_sc_yr, by = c("city_id", "city_name", "risk", "measure"))
      }
      count <- count + 1
    }
  }
}


#Tidy
temp_death_daly_pop <- temp_death_daly_pop %>%
  dplyr::select(c(city_id:count_baseline)) %>%
  left_join(temp, by = c("city_id", "city_name", "risk", "measure"))
  



################################################################################
#SECTION 8: SAVE AND CLOSE
################################################################################

if(run_choice == "main_run"){
  

  #Save data for main run
  if(tree_cover_increase > 0 & green_roof_increase > 0){
    saveRDS(temp_death_daly, "01_data/02_interim/05_heat/heat_health_impacts_no_pop_growth.rds")
    saveRDS(temp_death_daly_pop, "01_data/02_interim/05_heat/heat_health_impacts.rds")
  } else if(tree_cover_increase > 0){
    saveRDS(temp_death_daly, "01_data/02_interim/05_heat/heat_trees_only_health_impacts_no_pop_growth.rds")
    saveRDS(temp_death_daly_pop, "01_data/02_interim/05_heat/heat_trees_only_health_impacts.rds")
  } else if(green_roof_increase > 0){
    saveRDS(temp_death_daly, "01_data/02_interim/05_heat/heat_roofs_only_health_impacts_no_pop_growth.rds")
    saveRDS(temp_death_daly_pop, "01_data/02_interim/05_heat/heat_roofs_only_health_impacts.rds")
  } else {
    print("No heat measure defined!")
  }
  
  
  #Save urban-rural adjustment data
  saveRDS(urb_rur, "01_data/02_interim/05_heat/heat_urban_rural_adjustment_factors.rds")
  
  
  #If carrying out sensitivity analysis, save data for sensitivity analysis
} else{
  saveRDS(temp_death_daly, paste0("01_data/02_interim/05_heat/sensitivity_analysis/", run_choice, "_heat_health_impacts_no_pop_growth.rds"))
  saveRDS(temp_death_daly_pop, paste0("01_data/02_interim/05_heat/sensitivity_analysis/", run_choice, "_heat_health_impacts.rds"))
}


#Remove data that is no longer needed
rm(list = ls(pattern = "temp"), envir = .GlobalEnv)
rm(bl_health)


#Display completion messages
cat(green("\nModelling heat complete."))

