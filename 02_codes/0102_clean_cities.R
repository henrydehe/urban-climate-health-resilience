################################################################################
#NAME:         0102_clean_cities.R
#AUTOR:        Alessa Widmaier and co-authors
#DATE:         03/05/2026
#DESCRIPTION:  Clean city-level polygons and characteristics
#RUNNING TIME: ~10min
################################################################################


################################################################################
#SECTION 1: INITIATE
################################################################################

#Display starting message
cat(green("\n\nCleaning city data started."))




################################################################################
#SECTION 2: LOAD DATA
################################################################################

#2.1: Read in required data ----------------------------------------------------

#City-level polygons
cities <- terra::vect("01_data/01_raw/01_cities/GHSL_Cities_DB/GHS_UCDB_GLOBE_R2024A.gpkg", layer = "GHS_UCDB_THEME_CLIMATE_GLOBE_R2024A")


#Population growth data
pop_growth <- readxl::read_excel("01_data/01_raw/01_cities/WUP_Urban_Growth_Rate/WUP2018-F06-Urban_Growth_Rate.xls", skip = 16)


#Baseline heat-related deaths at city level to get country codes
bl_health <- readRDS("01_data/02_interim/02_health_baseline/city_level_disease.rds")


#Land cover shares of trees and built-up areas
trees <- terra::rast("01_data/01_raw/01_cities/landcover_shares/PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif")
builtup <- terra::rast("01_data/01_raw/01_cities/landcover_shares/PROBAV_LC100_global_v3.0.1_2019-nrt_BuiltUp-CoverFraction-layer_EPSG-4326.tif")




################################################################################
#SECTION 3: CLEAN DATA
################################################################################

#3.1: Clean city data ----------------------------------------------------------

#Reproject to CRS of choice
cities <- terra::project(cities, crs_choice)


#Keep only variables of interest
cities <- cities %>%
  tidyterra::select(c(city_id = ID_UC_G0, city_name = GC_UCN_MAI_2025,
                      country_name = GC_CNT_GAD_2025, region_name = GC_DEV_USR_2025, 
                      development_status = GC_DEV_WIG_2025, population = GC_POP_TOT_2025,
                      size_km2 = GC_UCA_KM2_2025))



#3.2: Get green and built-up areas ---------------------------------------------

#Get fraction of green and built-up areas for each city
trees <- terra::extract(trees, cities, fun = mean, na.rm = TRUE) %>% dplyr::select(c(id = 1, trees_share = 2))
builtup <- terra::extract(builtup, cities, fun = mean, na.rm = TRUE) %>% dplyr::select(c(id = 1, builtup_share = 2))


#Append to city dataframe
cities$trees_share <- trees$trees_share[match(seq_len(nrow(cities)), trees$id)]
cities$builtup_share <- builtup$builtup_share[match(seq_len(nrow(cities)), builtup$id)]
cities <- cities %>%
  tidyterra::mutate(trees_share = trees_share / 100,
                    builtup_share = builtup_share / 100)



#3.3: Clean population growth data ---------------------------------------------

#Select relevant columns
pop_growth <- pop_growth %>%
  dplyr::select(country_code = 4, rate_20_25 = 19, rate_25_30 = 20, rate_30_35 = 21,
                rate_35_40 = 22, rate_40_45 = 23, rate_45_50 = 24)


#Convert to multiplier rate
pop_growth <- pop_growth %>%
  mutate(across(c(2:ncol(pop_growth)), ~ (.x / 100) + 1))


#Convert country code
# The WUP file includes some territories, special areas and aggregate regions that do not map unambiguously to a single World Bank country code. These were previously converted to NA by countrycode() and then removed. Here, we remove them explicitly for transparency.
unmatched_wup_codes <- c(
  175, 184, 238, 254, 312, 336, 474, 500, 535, 570, 638, 654, 660, 666,
  732, 772, 830, 876, 900, 901, 902, 903, 904, 905, 906, 908, 909, 910,
  911, 912, 913, 914, 915, 916, 920, 921, 922, 923, 924, 925, 926, 927,
  928, 931, 934, 935, 941, 947, 948, 954, 957, 1500, 1501, 1502, 1503,
  1517, 5500, 5501
)

pop_growth <- pop_growth %>%
  mutate(country_code = as.integer(country_code)) %>%
  filter(!country_code %in% unmatched_wup_codes)

pop_growth$country_code <- countrycode::countrycode(
  pop_growth$country_code,
  origin = "iso3n",
  destination = "wb"
)

pop_growth <- pop_growth %>%
  filter(!is.na(country_code))


#Bring together with cities data
urb_growth <- bl_health %>%
  dplyr::select(c(city_id = unique_id, city_name = urban_centre_main_name, country_code = wb_country_code)) %>%
  distinct() %>%
  left_join(as.data.frame(cities), by = c("city_id", "city_name")) %>%
  dplyr::select(-c(region_name, development_status, size_km2, trees_share, builtup_share)) %>%
  left_join(pop_growth, by = c("country_code"))
  

#Calculate population growth by 2030, 2040 and 2050
urb_growth <- urb_growth %>%
  mutate(pop_multiplier_2030 = rate_20_25^5 * rate_25_30^5,
         pop_multiplier_2040 = pop_multiplier_2030 * rate_30_35^5 * rate_35_40^5,
         pop_multiplier_2050 = pop_multiplier_2040 * rate_40_45^5 * rate_45_50^5) %>%
  dplyr::select(c(city_id, city_name, population, pop_multiplier_2030:pop_multiplier_2050))




################################################################################
#SECTION 4: SAVE AND CLOSE
################################################################################

#Save cleaned data
writeVector(cities, "01_data/02_interim/01_cities/cities.gpkg", overwrite = TRUE)
saveRDS(urb_growth, "01_data/02_interim/01_cities/population_growth.rds")


#Remove data that is no longer needed
rm(cities, pop_growth, bl_health, urb_growth)


#Display completion messages
cat(green("\nCleaning city data complete."))
