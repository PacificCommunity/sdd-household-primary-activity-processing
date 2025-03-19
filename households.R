#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Load setup thus running the required libraries and create some subsidiary tables

source("src/setup.R")

households <- function(hhNum){
  #### ********************** Generate the total number of households *************************************************** ####
  
  pActivity_HH <- pActivity %>%
    group_by(countryCode, year, rururbCode, sexID, AGE) %>%
    summarise(households = round(sum(hhwt), 0))
  
  pActivity_HH <- as.data.table(pActivity_HH)
  pActivity_HH_cube <- cube(pActivity_HH, j = round(sum(households), 2), by = c("countryCode", "year", "rururbCode", "sexID", "AGE"), id = FALSE )
  
  pActivity_HH_cube <- pActivity_HH_cube %>%
    filter(!is.na(countryCode))
  
  pActivity_HH_cube <- pActivity_HH_cube %>%
    filter(!is.na(year)) %>%
    rename(households = V1)
  
  
  pActivity_HH_cube <- pActivity_HH_cube %>%
    mutate_all(~replace(., is.na(.), "_T")) %>%
    filter(rururbCode != "N")
  
  pActivity_HH_cube_DT <- pActivity_HH_cube %>%
    rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = households, SEX = sexID) %>%
    mutate(FREQ = "A", INDICATOR = "NHH", FOOD_ACTIVITY = "_T", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")
  
  #Sub_national data processing
  
  pActivity_HH_sn <- pActivity %>%
    group_by(strataID, year, rururbCode, sexID, AGE) %>%
    summarise(households = round(sum(hhwt), 0))
  
  pActivity_HH_sn <- as.data.table(pActivity_HH_sn)
  pActivity_HH_sn_cube <- cube(pActivity_HH_sn, j = round(sum(households), 2), by = c("strataID", "year", "rururbCode", "sexID", "AGE"), id = FALSE )
  
  pActivity_HH_sn_cube <- pActivity_HH_sn_cube %>%
    filter(!is.na(strataID))
  
  pActivity_HH_sn_cube <- pActivity_HH_sn_cube %>%
    filter(!is.na(year)) %>%
    rename(households = V1)
  
  pActivity_HH_sn_cube <- pActivity_HH_sn_cube %>%
    mutate_all(~replace(., is.na(.), "_T")) %>%
    filter(rururbCode != "N")
  
  pActivity_HH_sn_cube_DT <- pActivity_HH_sn_cube %>%
    rename(GEO_PICT=strataID, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = households, SEX = sexID) %>%
    mutate(FREQ = "A", INDICATOR = "NHH", FOOD_ACTIVITY = "_T", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")
  
  
  combine_hh <- rbind(pActivity_HH_cube_DT, pActivity_HH_sn_cube_DT)
  
  combine_hh <- combine_hh |>
    group_by(FREQ, TIME_PERIOD, GEO_PICT, URBANIZATION, SEX, AGE) |>
    summarise(totHH = sum(as.numeric(OBS_VALUE)))
  
  hhNum <-combine_hh
  
  return(hhNum)
  
}

