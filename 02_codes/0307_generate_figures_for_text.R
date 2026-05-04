################################################################################
#NAME:         0307_generate_figures_for_text.R
#AUTOR:        Alessa Widmaier and co-authors
#DATE:         03/05/2026
#DESCRIPTION:  Generate additional values used in the manuscript text
#RUNNING TIME: <1min
################################################################################


################################################################################
#SECTION 1: INITIATE
################################################################################

#Display starting message
cat(green("\n\nGenerating text figures started."))




################################################################################
#SECTION 2: LOAD DATA
################################################################################

#2.1: Read healthcare expenditure inputs -----------------------------------

#Calculate total current healthcare GDP globally

countries <- readRDS("01_data/02_interim/05_heat/heat_urban_rural.rds")
gdp <- read.csv("01_data/01_raw/08_cobenefits/healthcare_spending/wb_gdp_current_usd.csv", skip = 4)
healthshare <- read.csv("01_data/01_raw/08_cobenefits/healthcare_spending/wb_health_exp_pct_gdp.csv", skip = 4)


countries <- countries %>%
  dplyr::select(wb_country_code) %>%
  distinct()


gdp <- gdp %>%
  dplyr::select(c(wb_country_code = Country.Code, gdp = X2021))


healthshare <- healthshare %>%
  dplyr::select(c(wb_country_code = Country.Code, healthshare = X2021))


countries <- countries %>%
  left_join(gdp, by = c("wb_country_code")) %>%
  left_join(healthshare, by = c("wb_country_code")) %>%
  mutate(health_gdp = healthshare/100 * gdp)


paste0("Annual healthcare GDP globally in 2021 sums to US$", round(sum(countries$health_gdp, na.rm = TRUE)/10^9, 3), " billion.")
#Use this value to contextualise healthcare spending savings in the manuscript


#Display completion message
cat(green("\nGenerating text figures complete."))
