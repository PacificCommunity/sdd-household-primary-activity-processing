#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Load setup thus running the required libraries and create some subsidiary tables

source("src/setup.R")

#Creating Fishing location column

fishing <- pActivity

fishing <- fishing |>
  filter(fisheries == 1) |>
  mutate(fishing_location = case_when(
    fishloc_inshore==1 & fishloc_nearshore ==1 & fishloc_offshore ==1 & fishloc_other == 1 ~ "ALLLOC",
    
    fishloc_inshore==1 & fishloc_nearshore ==1 & fishloc_offshore ==1 & is.na(fishloc_other) ~ "ALLXOT",
    fishloc_inshore==1 & fishloc_nearshore ==1 & is.na(fishloc_offshore)  & fishloc_other == 1 ~ "ALLXOF",
    fishloc_inshore==1 & is.na(fishloc_nearshore)  & fishloc_offshore ==1 & fishloc_other == 1 ~ "ALLXNR",
    is.na(fishloc_inshore) & fishloc_nearshore ==1 & fishloc_offshore ==1 & fishloc_other == 1 ~ "ALLXIN",
    
    fishloc_inshore==1 & is.na(fishloc_nearshore) & is.na(fishloc_offshore) & is.na(fishloc_other) ~ "INONLY",
    is.na(fishloc_inshore) & fishloc_nearshore ==1 & is.na(fishloc_offshore) & is.na(fishloc_other) ~ "NRONLY",
    is.na(fishloc_inshore) & is.na(fishloc_nearshore) & fishloc_offshore ==1 & is.na(fishloc_other) ~ "OFONLY",
    is.na(fishloc_inshore) & is.na(fishloc_nearshore) & is.na(fishloc_offshore) & fishloc_other == 1 ~ "OTONLY",
    
    fishloc_inshore == 1 & fishloc_nearshore == 1 & is.na(fishloc_offshore) & is.na(fishloc_other) ~ "INNRONLY",
    fishloc_inshore == 1 & is.na(fishloc_nearshore) & fishloc_offshore==1 & is.na(fishloc_other) ~ "INOFONLY",
    fishloc_inshore == 1 & is.na(fishloc_nearshore) & is.na(fishloc_offshore) & fishloc_other ==1 ~ "INOTONLY",
    
    is.na(fishloc_inshore)  & fishloc_nearshore == 1 & fishloc_offshore==1 & is.na(fishloc_other) ~ "NROFONLY",
    is.na(fishloc_inshore)  & fishloc_nearshore ==1 & is.na(fishloc_offshore) & fishloc_other == 1 ~ "NROTONLY",
    
    is.na(fishloc_inshore)  & is.na(fishloc_nearshore) & fishloc_offshore==1 & fishloc_other ==1 ~ "OFOTONLY",
    
    is.na(fishloc_inshore) & is.na(fishloc_nearshore) & is.na(fishloc_offshore)  & is.na(fishloc_other) ~ "NSTD"
    
    ),
    
    fishing_method = case_when(
      fishmethod_gleaning ==1 & fishmethod_line ==1 & fishmethod_net ==1 & fishmethod_spear ==1 & fishmethod_other ==1 ~ "ALLMTH",
      
      fishmethod_gleaning ==1 & fishmethod_line ==1 & fishmethod_net ==1 & fishmethod_spear ==1 & is.na(fishmethod_other) ~ "ALLXOT",
      fishmethod_gleaning ==1 & fishmethod_line ==1 & fishmethod_net ==1 & is.na(fishmethod_spear) & fishmethod_other ==1 ~ "ALLXSP",
      fishmethod_gleaning ==1 & fishmethod_line ==1 & is.na(fishmethod_net) & fishmethod_spear ==1 & fishmethod_other ==1 ~ "ALLXNT",
      fishmethod_gleaning ==1 & is.na(fishmethod_line) & fishmethod_net==1 & fishmethod_spear ==1 & fishmethod_other ==1 ~ "ALLXLN",
      is.na(fishmethod_gleaning) & fishmethod_line ==1 & fishmethod_net==1 & fishmethod_spear ==1 & fishmethod_other ==1 ~ "ALLXGL",
      
      fishmethod_gleaning ==1 & is.na(fishmethod_line) & is.na(fishmethod_net) & is.na(fishmethod_spear) & is.na(fishmethod_other) ~ "GLONLY",
      is.na(fishmethod_gleaning) & fishmethod_line ==1 & is.na(fishmethod_net) & is.na(fishmethod_spear) & is.na(fishmethod_other) ~ "LNONLY",
      is.na(fishmethod_gleaning) & is.na(fishmethod_line) & fishmethod_net==1 & is.na(fishmethod_spear) & is.na(fishmethod_other) ~ "NTONLY",
      is.na(fishmethod_gleaning) & is.na(fishmethod_line) & is.na(fishmethod_net) & fishmethod_spear ==1 & is.na(fishmethod_other) ~ "SPONLY",
      is.na(fishmethod_gleaning) & is.na(fishmethod_line) & is.na(fishmethod_net) & is.na(fishmethod_spear) & fishmethod_other ==1 ~ "OTONLY",
      
      fishmethod_gleaning==1 & fishmethod_line ==1 & is.na(fishmethod_net) & is.na(fishmethod_spear) & is.na(fishmethod_other) ~ "GLLNONLY",
      fishmethod_gleaning==1 & is.na(fishmethod_line) & fishmethod_net==1 & is.na(fishmethod_spear) & is.na(fishmethod_other) ~ "GLNTONLY",
      fishmethod_gleaning==1 & is.na(fishmethod_line) & is.na(fishmethod_net) & fishmethod_spear==1 & is.na(fishmethod_other) ~ "GLSPONLY",
      fishmethod_gleaning==1 & is.na(fishmethod_line) & is.na(fishmethod_net) & is.na(fishmethod_spear) & fishmethod_other==1 ~ "GLOTONLY",
      
      is.na(fishmethod_gleaning) & fishmethod_line ==1 & fishmethod_net==1 & is.na(fishmethod_spear) & is.na(fishmethod_other) ~ "LNNTONLY",
      is.na(fishmethod_gleaning) & fishmethod_line ==1 & is.na(fishmethod_net) & fishmethod_spear==1 & is.na(fishmethod_other) ~ "LNSPONLY",
      is.na(fishmethod_gleaning) & fishmethod_line ==1 & is.na(fishmethod_net) & is.na(fishmethod_spear) & fishmethod_other==1 ~ "LNOTONLY",
      
      is.na(fishmethod_gleaning) & is.na(fishmethod_line) & fishmethod_net==1 & fishmethod_spear==1 & is.na(fishmethod_other) ~ "NTSPONLY",
      is.na(fishmethod_gleaning) & is.na(fishmethod_line) & fishmethod_net==1 & is.na(fishmethod_spear) & fishmethod_other==1 ~ "NTOTONLY",
      
      is.na(fishmethod_gleaning) & is.na(fishmethod_line) & is.na(fishmethod_net) & fishmethod_spear==1 & fishmethod_other==1 ~ "SPOTONLY",
      
      fishmethod_gleaning==1 & fishmethod_line==1 & fishmethod_net==1 & is.na(fishmethod_spear) & is.na(fishmethod_other) ~ "GLLNNTONLY",
      fishmethod_gleaning==1 & is.na(fishmethod_line) & fishmethod_net==1 & fishmethod_spear==1 & is.na(fishmethod_other) ~ "GLNTSPONLY",
      fishmethod_gleaning==1 & is.na(fishmethod_line) & is.na(fishmethod_net) & fishmethod_spear==1 & fishmethod_other==1 ~ "GLSPOTONLY",
      
      is.na(fishmethod_gleaning) & fishmethod_line==1 & fishmethod_net==1 & fishmethod_spear==1 & is.na(fishmethod_other) ~ "LNNTSPONLY",
      is.na(fishmethod_gleaning) & fishmethod_line==1 & is.na(fishmethod_net) & fishmethod_spear==1 & fishmethod_other==1 ~ "LNSPOTONLY",
      
      is.na(fishmethod_gleaning) & is.na(fishmethod_line) & fishmethod_net==1 & fishmethod_spear==1 & fishmethod_other==1 ~ "NTSPOTONLY",
      
      fishmethod_gleaning==1 & fishmethod_line==1 & is.na(fishmethod_net) & fishmethod_spear==1 & is.na(fishmethod_other) ~ "GLLNSPONLY",
      fishmethod_gleaning==1 & is.na(fishmethod_line) & fishmethod_net==1 & is.na(fishmethod_spear) & fishmethod_other==1 ~ "GLNTOTONLY",
      fishmethod_gleaning==1 & fishmethod_line==1 & is.na(fishmethod_net) & is.na(fishmethod_spear) & fishmethod_other==1 ~ "GLLNOTONLY",
      is.na(fishmethod_gleaning) & fishmethod_line==1 & fishmethod_net==1 & is.na(fishmethod_spear) & fishmethod_other==1 ~ "LNNTOTONLY",
      
      is.na(fishmethod_gleaning) & is.na(fishmethod_line) & is.na(fishmethod_net) & is.na(fishmethod_spear) & is.na(fishmethod_other) ~ "NSTD"

          )
    
    )


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
  mutate(FREQ = "A", INDICATOR = "NHH", FISHING_ACTIVITY = "_T", FISHING_LOCATION = "_T", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

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
  mutate(FREQ = "A", INDICATOR = "NHH", FISHING_ACTIVITY = "_T", FISHING_LOCATION = "_T", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Combine household records

combine_household <- rbind(pActivity_HH_cube_DT, pActivity_strata_HH_cube_DT)

#country household involve in inshore fishing
fishing_Country <- fishing_loc %>%
  #filter(fisheries == 1) |>
  group_by(countryCode, year, rururbCode, fish) %>%
  summarise(households = round(sum(hhwt), 0))

fishing_Country <- as.data.table(fishing_Country)
fishing_Country_cube <- cube(fishing_Country, j = round(sum(households), 2), by = c("countryCode", "year", "rururbCode", "fishing_location", "fishing_method"), id = FALSE )

fishing_Country_cube <- fishing_Country_cube %>%
  filter(!is.na(countryCode))

fishing_Country_cube <- fishing_Country_cube %>%
  filter(!is.na(year)) %>%
  rename(households = V1)

fishing_Country_cube <- fishing_Country_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>% 
  filter(rururbCode != "N")

fishing_Country_cube_DT <- fishing_Country_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = households, FISHING_ACTIVITY = fishing_method, FISHING_LOCATION = fishing_location) %>%
  mutate(FREQ = "A", INDICATOR = "NHH", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Strata household fishing location processing

fishing_strata <- fishing_loc %>%
  filter(fisheries == 1) |>
  group_by(strataID, year, rururbCode, fishing_location, fishing_method) %>%
  summarise(households = round(sum(hhwt), 0))

fishing_strata <- as.data.table(fishing_strata)
fishing_strata_cube <- cube(fishing_strata, j = round(sum(households), 2), by = c("strataID", "year", "rururbCode", "fishing_location", "fishing_method"), id = FALSE )

fishing_strata_cube <- fishing_strata_cube %>%
  filter(!is.na(strataID))

fishing_strata_cube <- fishing_strata_cube %>%
  filter(!is.na(year)) %>%
  rename(households = V1)

fishing_strata_cube <- fishing_strata_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>% 
  filter(rururbCode != "N")

fishing_strata_cube_DT <- fishing_strata_cube %>%
  rename(GEO_PICT=strataID, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = households, FISHING_ACTIVITY = fishing_method, FISHING_LOCATION = fishing_location) %>%
  mutate(FREQ = "A", INDICATOR = "NHH", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#combine inshore fishing location

fishing_combine <- rbind(fishing_Country_cube_DT, fishing_strata_cube_DT)

fishing_combine <- fishing_combine |>
  select(FREQ, TIME_PERIOD, GEO_PICT, URBANIZATION, INDICATOR, FISHING_ACTIVITY, FISHING_LOCATION, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, CONF_STATUS)


#Calculate percentages

combine_household_id <- combine_household 

combine_household_id$OBS_VALUE <- as.numeric(combine_household_id$OBS_VALUE) 

combine_household_id <- combine_household_id |>
  group_by(FREQ, TIME_PERIOD, GEO_PICT, URBANIZATION) |>
  summarise(totHH = sum(OBS_VALUE))


fishing_combine_merge_hh <- merge(fishing_combine, combine_household_id)
fishing_combine_merge_hh$OBS_VALUE <- as.numeric(fishing_combine_merge_hh$OBS_VALUE)

fishing_combine_merge_hh$percentage <- round(fishing_combine_merge_hh$OBS_VALUE/fishing_combine_merge_hh$totHH*100, 2)

fishing_combine_percent <- fishing_combine_merge_hh |>
  select(-OBS_VALUE, -totHH) |>
  rename(OBS_VALUE = percentage) |>
  mutate(INDICATOR = "PERCENT",
         UNIT_MEASURE = "PERCENT"
         )

fishing_combine_percent <- fishing_combine_percent |>
  select(FREQ, TIME_PERIOD, GEO_PICT, URBANIZATION, INDICATOR, FISHING_ACTIVITY, FISHING_LOCATION, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, CONF_STATUS)


#Finalise Fishing dataset

fishing_final <- rbind(fishing_combine, fishing_combine_percent)

#Write final fishing file to csv file

write.csv(fishing_final, "output/fisheries_data.csv", row.names = FALSE)
























