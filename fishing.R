#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Load setup thus running the required libraries and create some subsidiary tables

source("src/setup.R")

#### ******************************* Household number processing **************************** ####
#country household processing
pActivity_HH <- pActivity %>%
  group_by(countryCode, year, rururbCode) %>%
  summarise(households = round(sum(hhwt), 0))

pActivity_HH <- as.data.table(pActivity_HH)
pActivity_HH_cube <- cube(pActivity_HH, j = round(sum(households), 2), by = c("countryCode", "year", "rururbCode"), id = FALSE )

pActivity_HH_cube <- pActivity_HH_cube %>%
  filter(!is.na(countryCode))

pActivity_HH_cube <- pActivity_HH_cube %>%
  filter(!is.na(year)) %>%
  rename(households = V1)

pActivity_HH_cube <- pActivity_HH_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>% 
  filter(rururbCode != "N")

pActivity_HH_cube_DT <- pActivity_HH_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = households) %>%
  mutate(FREQ = "A", INDICATOR = "NHH", FOOD_ACTIVITY = "_T", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Strata household processing

pActivity_strata_HH <- pActivity %>%
  group_by(strataID, year, rururbCode) %>%
  summarise(households = round(sum(hhwt), 0))

pActivity_strata_HH <- as.data.table(pActivity_strata_HH)
pActivity_strata_HH_cube <- cube(pActivity_strata_HH, j = round(sum(households), 2), by = c("strataID", "year", "rururbCode"), id = FALSE )

pActivity_strata_HH_cube <- pActivity_strata_HH_cube %>%
  filter(!is.na(strataID))

pActivity_strata_HH_cube <- pActivity_strata_HH_cube %>%
  filter(!is.na(year)) %>%
  rename(households = V1)

pActivity_strata_HH_cube <- pActivity_strata_HH_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>% 
  filter(rururbCode != "N")

pActivity_strata_HH_cube_DT <- pActivity_strata_HH_cube %>%
  rename(GEO_PICT=strataID, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = households) %>%
  mutate(FREQ = "A", INDICATOR = "NHH", FOOD_ACTIVITY = "_T", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")





