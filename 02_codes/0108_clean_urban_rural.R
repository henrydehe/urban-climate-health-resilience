################################################################################
#NAME:         0108_clean_urban_rural.R
#AUTOR:        Alessa Widmaier and co-authors
#DATE:         03/05/2026
#DESCRIPTION:  Estimate urban-rural exposure differences for heat, pollution,
#              WASH, and BMI
#RUNNING TIME: ~30in
################################################################################


################################################################################
#SECTION 1: INITIATE
################################################################################

#Display starting message
cat(green("\n\nCleaning urban vs rural data started."))




################################################################################
#SECTION 2: LOAD DATA
################################################################################

#2.1: Read in geographical data ------------------------------------------------

#Country outlines
countries <- terra::vect("01_data/01_raw/01_cities/country_boundaries/World Bank Official Boundaries - Admin 0.gpkg")


#City outlines
cities <- terra::vect("01_data/01_raw/01_cities/GHSL_Cities_DB/GHS_UCDB_GLOBE_R2024A.gpkg", layer = "GHS_UCDB_THEME_CLIMATE_GLOBE_R2024A")


#City-country matching
city_country <- readRDS("01_data/02_interim/02_health_baseline/city_level_disease.rds")



#2.2: Read in baseline data ----------------------------------------------------

#Population map
pop <- terra::rast("01_data/01_raw/01_cities/WorldPop/global_pop_2020_CN_1km_R2025A_UA_v1.tif")


#Baseline temperature map
temp <- terra::rast("01_data/01_raw/05_heat/CHELSA_bio01_1981-2010_V.2.1.tif")
temp_sd <- terra::rast("01_data/01_raw/05_heat/CHELSA_bio04_1981-2010_V.2.1.tif")


#Urban heat island effect
uhi_ids <- read.csv("01_data/01_raw/05_heat/UHI/CityInfo.csv")
uhi <- read.csv("01_data/01_raw/05_heat/UHI/UHII_dataset/SAT_Day_2020_30.csv")


#Baseline air quality map
airqual <- terra::rast("01_data/01_raw/04_air_quality/V6GL02.04.CNNPM25.GL.202001-202012.nc")
airqual_cities <- readxl::read_excel("01_data/01_raw/01_cities/GHSL_Cities_DB/GHS_UCDB_GLOBE_R2024A.xlsx", sheet = "EMISSIONS")




################################################################################
#SECTION 3: CLEAN DATA
################################################################################

#3.1: Basic clean --------------------------------------------------------------

#Country outlines
countries <- countries %>%
  tidyterra::select(c(wb_country_code = ISO_A3)) %>%
  aggregate(by = "wb_country_code") %>%
  tidyterra::select(c(wb_country_code))


#City outlines
cities <- cities %>%
  tidyterra::select(c(city_id = 1, city_name = 2)) %>%
  project(crs(countries))


#City-country matching
city_country <- city_country %>%
  dplyr::select(c(city_id = 1, city_name = 2, wb_country_code)) %>%
  mutate(wb_country_code = ifelse(wb_country_code == "TWN", "CHN", wb_country_code)) %>%
  distinct()


#Clean UHI dataset
uhi <- uhi %>%
  left_join(uhi_ids, by = c("UrbanId")) %>%
  dplyr::select(uhi_id = "UrbanId", lon = "Longitude", lat = "Latitude", uhi_effect = "Intensity_DEA")
uhi <- vect(uhi, geom = c("lon", "lat"), crs = crs_choice)



#3.2: Population ---------------------------------------------------------------

#Country level
pop_countries <- terra::extract(pop, countries, fun = sum, na.rm = TRUE)
pop_countries <- cbind(countries, pop_countries) %>%
  dplyr::select(c(wb_country_code, pop_country = 3)) %>%
  as.data.frame()


#City level
pop_cities <- terra::extract(pop, cities, fun = sum, na.rm = TRUE)
pop_cities <- cbind(cities, pop_cities) %>%
  dplyr::select(c(city_id, city_name, pop_city = 4)) %>%
  as.data.frame()


#Bring together
city_country <- city_country %>%
  left_join(pop_countries, by = c("wb_country_code")) %>%
  left_join(pop_cities, by = c("city_id", "city_name"))



#3.3: Temperature --------------------------------------------------------------

#Temp x population map
temp_pop <- temp * resample(pop, temp, method = "bilinear")
temp_sd_pop <- temp_sd * resample(pop, temp_sd, method = "bilinear")


#Country level
temp_countries <- terra::extract(temp_pop, countries, fun = sum, na.rm = TRUE)
temp_countries <- cbind(countries, temp_countries) %>%
  dplyr::select(c(wb_country_code, temp_country = 3)) %>%
  as.data.frame()
temp_sd_countries <- terra::extract(temp_sd_pop, countries, fun = sum, na.rm = TRUE)
temp_sd_countries <- cbind(countries, temp_sd_countries) %>%
  dplyr::select(c(wb_country_code, temp_sd_country = 3)) %>%
  as.data.frame()


#City level
temp_cities <- terra::extract(temp_pop, cities, fun = sum, na.rm = TRUE)
temp_cities <- cbind(cities, temp_cities) %>%
  dplyr::select(c(city_id, city_name, temp_city = 4)) %>%
  as.data.frame()
temp_sd_cities <- terra::extract(temp_sd_pop, cities, fun = sum, na.rm = TRUE)
temp_sd_cities <- cbind(cities, temp_sd_cities) %>%
  dplyr::select(c(city_id, city_name, temp_sd_city = 4)) %>%
  as.data.frame()


#Add UHI effect - match each city with closest city in UHI dataset, then add UHI to temperature
nn <- nearby(cities, uhi, k = 1, centroids = TRUE)
uhi_cities <- cities %>%
  as.data.frame() %>%
  cbind(nn) %>%
  left_join(as.data.frame(uhi), by = c("k1" = "uhi_id")) %>%
  dplyr::select(c(city_id, city_name, uhi_effect))


#Bring together
temp_city_country <- city_country %>%
  left_join(temp_countries, by = c("wb_country_code")) %>%
  left_join(temp_sd_countries, by = c("wb_country_code")) %>%
  left_join(temp_cities, by = c("city_id", "city_name")) %>%
  left_join(temp_sd_cities, by = c("city_id", "city_name")) %>%
  left_join(uhi_cities, by = c("city_id", "city_name")) %>%
  filter(!is.na(temp_city)) %>%
  mutate(temp_country = temp_country / pop_country,
         temp_sd_country = temp_sd_country / pop_country / 100,
         temp_city = temp_city / pop_city + uhi_effect,
         temp_sd_city = temp_sd_city / pop_city / 100) %>%
  dplyr::select(-c(uhi_effect))



#3.4: Air quality --------------------------------------------------------------

#Reproject
airqual <- project(airqual, crs(pop))


#Air quality x population map
airqual_pop <- airqual * resample(pop, airqual, method = "bilinear")


#Country level
airqual_countries <- terra::extract(airqual_pop, countries, fun = sum, na.rm = TRUE)
airqual_countries <- cbind(countries, airqual_countries) %>%
  dplyr::select(c(wb_country_code, airqual_country = 3)) %>%
  as.data.frame()


#City level
airqual_cities <- airqual_cities %>%
  rename(airqual_city = paste0("EM_PM2_CON_", year_baseline)) %>%
  dplyr::select(city_id = ID_UC_G0, city_name = GC_UCN_MAI_2025, airqual_city)


#Bring together
airqual_city_country <- city_country %>%
  left_join(airqual_countries, by = c("wb_country_code")) %>%
  left_join(airqual_cities, by = c("city_id", "city_name")) %>%
  filter(!is.na(airqual_city)) %>%
  mutate(airqual_country = airqual_country / pop_country)




################################################################################
#SECTION 4: SAVE AND CLOSE
################################################################################

#Save cleaned data
saveRDS(temp_city_country, "01_data/02_interim/05_heat/heat_urban_rural.rds")
saveRDS(airqual_city_country, "01_data/02_interim/04_air_quality/airqual_urban_rural.rds")


#Display completion messages
cat(green("\nCleaning urban vs rural data complete."))

