#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Load setup thus running the required libraries and create some subsidiary tables

source("src/setup.R")

#Load data file
pActivity <- read_dta("raw_data/SPC_REG-12_2012-2021_PRIMARY-ACTIVITIES.dta") %>%
             mutate(across(where(labelled::is.labelled), haven::as_factor))

#### ************************* Creating subsidiary tables to be merged with main table *********************************** ####

pActivity <- pActivity |>
  mutate(strataID = case_when(
                              country=="Cook Islands" & strata == 1 ~ "CK-1",
                              country=="Cook Islands" & strata == 2 ~ "CK-2",
                              country=="Fed. States of Micronesia" & strata == 1 ~ "FM-1",
                              country=="Fed. States of Micronesia" & strata == 2 ~ "FM-2",
                              country=="Kiribati" & strata == 1 ~ "KI-A",
                              country=="Kiribati" & strata == 2 ~ "KI-B",
                              country=="Kiribati" & strata == 3 ~ "KI-C",
                              country=="Kiribati" & strata == 4 ~ "KI-D",
                              country=="Kiribati" & strata == 5 ~ "KI-E",
                              country=="Marshall Islands" & strata == 1 ~ "MH-1",
                              country=="Marshall Islands" & strata == 2 ~ "MH-2",
                              country=="Marshall Islands" & strata == 3 ~ "MH-3",
                              country=="Marshall Islands" & strata == 4 ~ "MH-4",
                              country=="Marshall Islands" & strata == 5 ~ "MH-5",
                              country=="Marshall Islands" & strata == 6 ~ "MH-6",
                              country=="Marshall Islands" & strata == 1 ~ "MH-1",
                              country=="Palau" & strata == 1 ~ "PW-1",
                              country=="Palau" & strata == 2 ~ "PW-2",
                              country=="Tonga" & strata == 1 ~ "TO-1",
                              country=="Tonga" & strata == 2 ~ "TO-2",
                              country=="Tonga" & strata == 3 ~ "TO-3",
                              country=="Tonga" & strata == 4 ~ "TO-4",
                              country=="Tonga" & strata == 5 ~ "TO-5",
                              country=="Tonga" & strata == 6 ~ "TO-6",
                              country=="Tuvalu" & strata == 1 ~ "TV-A",
                              country=="Tuvalu" & strata == 2 ~ "TV-B",
                              country=="Vanuatu" & strata == 1 ~ "VU-1",
                              country=="Vanuatu" & strata == 2 ~ "VU-2",
                              country=="Vanuatu" & strata == 3 ~ "VU-3",
                              country=="Vanuatu" & strata == 4 ~ "VU-4",
                              country=="Vanuatu" & strata == 5 ~ "VU-5",
                              country=="Vanuatu" & strata == 6 ~ "VU-6",
                              country=="Vanuatu" & strata == 7 ~ "VU-7",
                              country=="Vanuatu" & strata == 8 ~ "VU-8",
                              country=="Wallis & Futuna" & strata == 1 ~ "WF-1",
                              country=="Wallis & Futuna" & strata == 2 ~ "WF-2",
                              country=="Nauru" & strata == 99 ~ "NR",
                              country=="Niue" & strata == 99 ~ "NU",
                              country=="Tokelau" & strata == 99 ~ "TK",
                              
                              ),
         
         AGE = case_when(
           age_grp1 == "15-19 years" ~ "Y15T24",
           age_grp1 == "20-24 years" ~ "Y15T24",
           age_grp1 == "25-29 years" ~ "Y25T59",
           age_grp1 == "30-34 years" ~ "Y25T59",
           age_grp1 == "35-39 years" ~ "Y25T59",
           age_grp1 == "40-44 years" ~ "Y25T59",
           age_grp1 == "45-49 years" ~ "Y25T59",
           age_grp1 == "50-54 years" ~ "Y25T59",
           age_grp1 == "55-59 years" ~ "Y25T59",
           age_grp1 == "60-64 years" ~ "Y60T999",
           age_grp1 == "65-69 years" ~ "Y60T999",
           age_grp1 == "70-74 years" ~ "Y60T999",
           age_grp1 == "75 years and older" ~ "Y60T999",
         ),
         
         sexID = ifelse(sex == "male", "M",
                        ifelse(sex == "female", "F", "N")),
         
         povertyID = case_when(national_povertyline == 1 ~ "POV",
                               national_povertyline == 0 ~ "NOP",
                               TRUE ~ "NDA"
                               )
  )

pActivity <- merge(pActivity, rururb, by = "rururb")
pActivity <- merge(pActivity, country, by = "country")

#### ********************** Generate the total number of households *************************************************** ####

pActivity_HH <- pActivity %>%
  group_by(countryCode, year, rururbCode, sexID, AGE, povertyID) %>%
  summarise(households = round(sum(hhwt), 0))

pActivity_HH <- as.data.table(pActivity_HH)
pActivity_HH_cube <- cube(pActivity_HH, j = round(sum(households), 2), by = c("countryCode", "year", "rururbCode", "sexID", "AGE", "povertyID"), id = FALSE )

pActivity_HH_cube <- pActivity_HH_cube %>%
  filter(!is.na(countryCode))

pActivity_HH_cube <- pActivity_HH_cube %>%
  filter(!is.na(year)) %>%
  rename(households = V1)


pActivity_HH_cube <- pActivity_HH_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_HH_cube_DT <- pActivity_HH_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = households, SEX = sexID, POVERTY = povertyID) %>%
  mutate(FREQ = "A", INDICATOR = "NHH", FOOD_ACTIVITY = "_T", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Sub_national data processing

pActivity_HH_sn <- pActivity %>%
  group_by(strataID, year, rururbCode, sexID, AGE, povertyID) %>%
  summarise(households = round(sum(hhwt), 0))

pActivity_HH_sn <- as.data.table(pActivity_HH_sn)
pActivity_HH_sn_cube <- cube(pActivity_HH_sn, j = round(sum(households), 2), by = c("strataID", "year", "rururbCode", "sexID", "AGE", "povertyID"), id = FALSE )

pActivity_HH_sn_cube <- pActivity_HH_sn_cube %>%
  filter(!is.na(strataID))

pActivity_HH_sn_cube <- pActivity_HH_sn_cube %>%
  filter(!is.na(year)) %>%
  rename(households = V1)

pActivity_HH_sn_cube <- pActivity_HH_sn_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_HH_sn_cube_DT <- pActivity_HH_sn_cube %>%
  rename(GEO_PICT=strataID, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = households, SEX = sexID, POVERTY = povertyID) %>%
  mutate(FREQ = "A", INDICATOR = "NHH", FOOD_ACTIVITY = "_T", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")
    
#### ****************************** Generate the number of households involved in livestock ************************* ####

#Generate Livestock households

pActivity_livestock <- pActivity %>%
  filter(livestock == 1) %>%
  group_by(countryCode, year, rururbCode, sexID, AGE, povertyID) %>%
  summarise(livestock_HH = round(sum(hhwt), 0))

pActivity_livestock <- as.data.table(pActivity_livestock)

pActivity_livestock_cube <- cube(pActivity_livestock, j = round(sum(livestock_HH), 2), by = c("countryCode", "year", "rururbCode", "sexID", "AGE", "povertyID"), id = FALSE )

pActivity_livestock_cube <- pActivity_livestock_cube %>%
  filter(!is.na(countryCode))

pActivity_livestock_cube <- pActivity_livestock_cube %>%
  filter(!is.na(year)) %>%
  rename(livestock_hh = V1)

pActivity_livestock_cube <- pActivity_livestock_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_livestock_cube_DT <- pActivity_livestock_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = livestock_hh, SEX = sexID, POVERTY = povertyID) %>%
  mutate(FREQ = "A", INDICATOR = "NHH", FOOD_ACTIVITY = "LIVE", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Generate sub national Livestock households

pActivity_livestock_sn <- pActivity %>%
  filter(livestock == 1) %>%
  group_by(strataID, year, rururbCode, sexID, AGE, povertyID) %>%
  summarise(livestock_HH = round(sum(hhwt), 0))

pActivity_livestock_sn <- as.data.table(pActivity_livestock_sn)

pActivity_livestock_sn_cube <- cube(pActivity_livestock_sn, j = round(sum(livestock_HH), 2), by = c("strataID", "year", "rururbCode", "sexID", "AGE", "povertyID"), id = FALSE )

pActivity_livestock_sn_cube <- pActivity_livestock_sn_cube %>%
  filter(!is.na(strataID))

pActivity_livestock_sn_cube <- pActivity_livestock_sn_cube %>%
  filter(!is.na(year)) %>%
  rename(livestock_hh = V1)

pActivity_livestock_sn_cube <- pActivity_livestock_sn_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_livestock_sn_cube_DT <- pActivity_livestock_sn_cube %>%
  rename(GEO_PICT=strataID, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = livestock_hh, SEX = sexID, POVERTY = povertyID) %>%
  mutate(FREQ = "A", INDICATOR = "NHH", FOOD_ACTIVITY = "LIVE", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Generate Fisheries households

pActivity_fisheries <- pActivity %>%
  filter(fisheries == 1) %>%
  group_by(countryCode, year, rururbCode, sexID, AGE, povertyID) %>%
  summarise(fisheries_HH = round(sum(hhwt), 0))

pActivity_fisheries <- as.data.table(pActivity_fisheries)

pActivity_fisheries_cube <- cube(pActivity_fisheries, j = round(sum(fisheries_HH), 0), by = c("countryCode", "year", "rururbCode", "sexID", "AGE", "povertyID"), id = FALSE )

pActivity_fisheries_cube <- pActivity_fisheries_cube %>%
  filter(!is.na(countryCode))

pActivity_fisheries_cube <- pActivity_fisheries_cube %>%
  filter(!is.na(year)) %>%
  rename(fisheries_hh = V1)

pActivity_fisheries_cube <- pActivity_fisheries_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_fisheries_cube_DT <- pActivity_fisheries_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = fisheries_hh, SEX = sexID, POVERTY = povertyID) %>%
  mutate(FREQ = "A", INDICATOR = "NHH", FOOD_ACTIVITY = "FISH", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Generate Fisheries households subnational

pActivity_fisheries_sn <- pActivity %>%
  filter(fisheries == 1) %>%
  group_by(strataID, year, rururbCode, sexID, AGE, povertyID) %>%
  summarise(fisheries_HH = round(sum(hhwt), 0))

pActivity_fisheries_sn <- as.data.table(pActivity_fisheries_sn)

pActivity_fisheries_sn_cube <- cube(pActivity_fisheries_sn, j = round(sum(fisheries_HH), 0), by = c("strataID", "year", "rururbCode", "sexID", "AGE", "povertyID"), id = FALSE )

pActivity_fisheries_sn_cube <- pActivity_fisheries_sn_cube %>%
  filter(!is.na(strataID))

pActivity_fisheries_sn_cube <- pActivity_fisheries_sn_cube %>%
  filter(!is.na(year)) %>%
  rename(fisheries_hh = V1)

pActivity_fisheries_sn_cube <- pActivity_fisheries_sn_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_fisheries_sn_cube_DT <- pActivity_fisheries_sn_cube %>%
  rename(GEO_PICT=strataID, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = fisheries_hh, SEX = sexID, POVERTY = povertyID) %>%
  mutate(FREQ = "A", INDICATOR = "NHH", FOOD_ACTIVITY = "FISH", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Generate Agriculture households

pActivity_agriculture <- pActivity %>%
  filter(agric == 1) %>%
  group_by(countryCode, year, rururbCode, sexID, AGE, povertyID) %>%
  summarise(agriculture_HH = round(sum(hhwt), 0))

pActivity_agriculture <- as.data.table(pActivity_agriculture)

pActivity_agriculture_cube <- cube(pActivity_agriculture, j = round(sum(agriculture_HH), 0), by = c("countryCode", "year", "rururbCode", "sexID", "AGE", "povertyID"), id = FALSE )

pActivity_agriculture_cube <- pActivity_agriculture_cube %>%
  filter(!is.na(countryCode))

pActivity_agriculture_cube <- pActivity_agriculture_cube %>%
  filter(!is.na(year)) %>%
  rename(agriculture_hh = V1)

pActivity_agriculture_cube <- pActivity_agriculture_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_agriculture_cube_DT <- pActivity_agriculture_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = agriculture_hh, SEX = sexID, POVERTY = povertyID) %>%
  mutate(FREQ = "A", INDICATOR = "NHH",  FOOD_ACTIVITY = "CROP", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Generate subnational Agriculture households

pActivity_agriculture_sn <- pActivity %>%
  filter(agric == 1) %>%
  group_by(strataID, year, rururbCode, sexID, AGE, povertyID) %>%
  summarise(agriculture_HH = round(sum(hhwt), 0))

pActivity_agriculture_sn <- as.data.table(pActivity_agriculture_sn)

pActivity_agriculture_sn_cube <- cube(pActivity_agriculture_sn, j = round(sum(agriculture_HH), 0), by = c("strataID", "year", "rururbCode", "sexID", "AGE", "povertyID"), id = FALSE )

pActivity_agriculture_sn_cube <- pActivity_agriculture_sn_cube %>%
  filter(!is.na(strataID))

pActivity_agriculture_sn_cube <- pActivity_agriculture_sn_cube %>%
  filter(!is.na(year)) %>%
  rename(agriculture_hh = V1)

pActivity_agriculture_sn_cube <- pActivity_agriculture_sn_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_agriculture_sn_cube_DT <- pActivity_agriculture_sn_cube %>%
  rename(GEO_PICT=strataID, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = agriculture_hh, SEX = sexID, POVERTY = povertyID) %>%
  mutate(FREQ = "A", INDICATOR = "NHH", FOOD_ACTIVITY = "CROP", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")


#Merging the households, livestock households, fisheries households and agriculture households 

primary_activity_households <- rbind(
  pActivity_HH_cube_DT,
  pActivity_HH_sn_cube_DT,
  pActivity_livestock_cube_DT,
  pActivity_livestock_sn_cube_DT,
  pActivity_fisheries_cube_DT,
  pActivity_fisheries_sn_cube_DT,
  pActivity_agriculture_cube_DT,
  pActivity_agriculture_sn_cube_DT
)

#new_order <- c("FREQ", "TIME_PERIOD", "GEO_PICT", "INDICATOR", "URBANIZATION", "OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "OBS_STATUS", "DATA_SOURCE", "OBS_COMMENT", "CONF_STATUS")
primary_activity_households_final <- primary_activity_households %>%
  select(FREQ, TIME_PERIOD, GEO_PICT, INDICATOR, FOOD_ACTIVITY, URBANIZATION, AGE, SEX, POVERTY, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, CONF_STATUS)
  

#### ************************** Run table of food activity ONLY ****************************** ####

#Generate Livestock households

pActivity_livestock_only <- pActivity %>%
  filter(livestock == 1 & is.na(fisheries) & is.na(agric)) %>%
  group_by(countryCode, year, rururbCode, sexID, AGE, povertyID) %>%
  summarise(livestock_HH = round(sum(hhwt), 0))

pActivity_livestock_only <- as.data.table(pActivity_livestock_only)

pActivity_livestock_only_cube <- cube(pActivity_livestock_only, j = round(sum(livestock_HH), 2), by = c("countryCode", "year", "rururbCode", "sexID", "AGE", "povertyID"), id = FALSE )

pActivity_livestock_only_cube <- pActivity_livestock_only_cube %>%
  filter(!is.na(countryCode))

pActivity_livestock_only_cube <- pActivity_livestock_only_cube %>%
  filter(!is.na(year)) %>%
  rename(livestock_hh = V1)

pActivity_livestock_only_cube <- pActivity_livestock_only_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_livestock_only_cube_DT <- pActivity_livestock_only_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = livestock_hh, SEX = sexID, POVERTY = povertyID) %>%
  mutate(FREQ = "A", INDICATOR = "NHH", FOOD_ACTIVITY = "LIVE_ONLY", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Generate sub national Livestock households

pActivity_livestock_sn_only <- pActivity %>%
  filter(livestock == 1 & is.na(fisheries) & is.na(agric)) %>%
  group_by(strataID, year, rururbCode, sexID, AGE, povertyID) %>%
  summarise(livestock_HH = round(sum(hhwt), 0))

pActivity_livestock_sn_only <- as.data.table(pActivity_livestock_sn_only)

pActivity_livestock_sn_only_cube <- cube(pActivity_livestock_sn_only, j = round(sum(livestock_HH), 2), by = c("strataID", "year", "rururbCode", "sexID", "AGE", "povertyID"), id = FALSE )

pActivity_livestock_sn_only_cube <- pActivity_livestock_sn_only_cube %>%
  filter(!is.na(strataID))

pActivity_livestock_sn_only_cube <- pActivity_livestock_sn_only_cube %>%
  filter(!is.na(year)) %>%
  rename(livestock_hh = V1)

pActivity_livestock_sn_only_cube <- pActivity_livestock_sn_only_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_livestock_sn_only_cube_DT <- pActivity_livestock_sn_only_cube %>%
  rename(GEO_PICT=strataID, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = livestock_hh, SEX = sexID, POVERTY = povertyID) %>%
  mutate(FREQ = "A", INDICATOR = "NHH", FOOD_ACTIVITY = "LIVE_ONLY", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Generate Fisheries households

pActivity_fisheries_only <- pActivity %>%
  filter(fisheries == 1 & is.na(livestock) & is.na(agric)) %>%
  group_by(countryCode, year, rururbCode, sexID, AGE, povertyID) %>%
  summarise(fisheries_HH = round(sum(hhwt), 0))

pActivity_fisheries_only <- as.data.table(pActivity_fisheries_only)

pActivity_fisheries_only_cube <- cube(pActivity_fisheries_only, j = round(sum(fisheries_HH), 0), by = c("countryCode", "year", "rururbCode", "sexID", "AGE", "povertyID"), id = FALSE )

pActivity_fisheries_only_cube <- pActivity_fisheries_only_cube %>%
  filter(!is.na(countryCode))

pActivity_fisheries_only_cube <- pActivity_fisheries_only_cube %>%
  filter(!is.na(year)) %>%
  rename(fisheries_hh = V1)

pActivity_fisheries_only_cube <- pActivity_fisheries_only_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_fisheries_only_cube_DT <- pActivity_fisheries_only_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = fisheries_hh, SEX = sexID, POVERTY = povertyID) %>%
  mutate(FREQ = "A", INDICATOR = "NHH", FOOD_ACTIVITY = "FISH_ONLY", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Generate Fisheries households subnational

pActivity_fisheries_sn_only <- pActivity %>%
  filter(fisheries == 1 & is.na(livestock) & is.na(agric)) %>%
  group_by(strataID, year, rururbCode, sexID, AGE, povertyID) %>%
  summarise(fisheries_HH = round(sum(hhwt), 0))

pActivity_fisheries_sn_only <- as.data.table(pActivity_fisheries_sn_only)

pActivity_fisheries_sn_only_cube <- cube(pActivity_fisheries_sn_only, j = round(sum(fisheries_HH), 0), by = c("strataID", "year", "rururbCode", "sexID", "AGE", "povertyID"), id = FALSE )

pActivity_fisheries_sn_only_cube <- pActivity_fisheries_sn_only_cube %>%
  filter(!is.na(strataID))

pActivity_fisheries_sn_only_cube <- pActivity_fisheries_sn_only_cube %>%
  filter(!is.na(year)) %>%
  rename(fisheries_hh = V1)

pActivity_fisheries_sn_only_cube <- pActivity_fisheries_sn_only_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_fisheries_sn_only_cube_DT <- pActivity_fisheries_sn_only_cube %>%
  rename(GEO_PICT=strataID, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = fisheries_hh, SEX = sexID, POVERTY = povertyID) %>%
  mutate(FREQ = "A", INDICATOR = "NHH", FOOD_ACTIVITY = "FISH_ONLY", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Generate Agriculture households

pActivity_agriculture_only <- pActivity %>%
  filter(agric == 1 & is.na(fisheries) & is.na(livestock)) %>%
  group_by(countryCode, year, rururbCode, sexID, AGE, povertyID) %>%
  summarise(agriculture_HH = round(sum(hhwt), 0))

pActivity_agriculture_only <- as.data.table(pActivity_agriculture_only)

pActivity_agriculture_only_cube <- cube(pActivity_agriculture_only, j = round(sum(agriculture_HH), 0), by = c("countryCode", "year", "rururbCode", "sexID", "AGE", "povertyID"), id = FALSE )

pActivity_agriculture_only_cube <- pActivity_agriculture_only_cube %>%
  filter(!is.na(countryCode))

pActivity_agriculture_only_cube <- pActivity_agriculture_only_cube %>%
  filter(!is.na(year)) %>%
  rename(agriculture_hh = V1)

pActivity_agriculture_only_cube <- pActivity_agriculture_only_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_agriculture_only_cube_DT <- pActivity_agriculture_only_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = agriculture_hh, SEX = sexID, POVERTY = povertyID) %>%
  mutate(FREQ = "A", INDICATOR = "NHH",  FOOD_ACTIVITY = "CROP_ONLY", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Generate subnational Agriculture households

pActivity_agriculture_sn_only <- pActivity %>%
  filter(agric == 1 & is.na(livestock) & is.na(fisheries)) %>%
  group_by(strataID, year, rururbCode, sexID, AGE, povertyID) %>%
  summarise(agriculture_HH = round(sum(hhwt), 0))

pActivity_agriculture_sn_only <- as.data.table(pActivity_agriculture_sn_only)

pActivity_agriculture_sn_only_cube <- cube(pActivity_agriculture_sn_only, j = round(sum(agriculture_HH), 0), by = c("strataID", "year", "rururbCode", "sexID", "AGE", "povertyID"), id = FALSE )

pActivity_agriculture_sn_only_cube <- pActivity_agriculture_sn_only_cube %>%
  filter(!is.na(strataID))

pActivity_agriculture_sn_only_cube <- pActivity_agriculture_sn_only_cube %>%
  filter(!is.na(year)) %>%
  rename(agriculture_hh = V1)

pActivity_agriculture_sn_only_cube <- pActivity_agriculture_sn_only_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_agriculture_sn_only_cube_DT <- pActivity_agriculture_sn_only_cube %>%
  rename(GEO_PICT=strataID, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = agriculture_hh, SEX = sexID, POVERTY = povertyID) %>%
  mutate(FREQ = "A", INDICATOR = "NHH", FOOD_ACTIVITY = "CROP_ONLY", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Merging the households, livestock households, fisheries households and agriculture households 

primary_activity_households_only <- rbind(
  pActivity_livestock_only_cube_DT,
  pActivity_livestock_sn_only_cube_DT,
  pActivity_fisheries_only_cube_DT,
  pActivity_fisheries_sn_only_cube_DT,
  pActivity_agriculture_only_cube_DT,
  pActivity_agriculture_sn_only_cube_DT
  
)

#new_order <- c("FREQ", "TIME_PERIOD", "GEO_PICT", "INDICATOR", "URBANIZATION", "OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "OBS_STATUS", "DATA_SOURCE", "OBS_COMMENT", "CONF_STATUS")
primary_activity_households_only_final <- primary_activity_households_only %>%
  select(FREQ, TIME_PERIOD, GEO_PICT, INDICATOR, FOOD_ACTIVITY, URBANIZATION, AGE, SEX, POVERTY, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, CONF_STATUS)

#### ************************ Households with no primary activity involvement **************************** ####

pActivity_none <- pActivity %>%
  filter(is.na(livestock) & is.na(fisheries) & is.na(agric)) %>%
  group_by(countryCode, year, rururbCode, sexID, AGE, povertyID) %>%
  summarise(livestock_HH = round(sum(hhwt), 0))

pActivity_none <- as.data.table(pActivity_none)

pActivity_none_cube <- cube(pActivity_none, j = round(sum(livestock_HH), 2), by = c("countryCode", "year", "rururbCode", "sexID", "AGE", "povertyID"), id = FALSE )

pActivity_none_cube <- pActivity_none_cube %>%
  filter(!is.na(countryCode))

pActivity_none_cube <- pActivity_none_cube %>%
  filter(!is.na(year)) %>%
  rename(livestock_hh = V1)

pActivity_none_cube <- pActivity_none_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_none_cube_DT <- pActivity_none_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = livestock_hh, SEX = sexID, POVERTY = povertyID) %>%
  mutate(FREQ = "A", INDICATOR = "NHH", FOOD_ACTIVITY = "NONE", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

# Sub-national table of those not involve in any activity 

pActivity_none_sn <- pActivity %>%
  filter(is.na(livestock) & is.na(fisheries) & is.na(agric)) %>%
  group_by(strataID, year, rururbCode, sexID, AGE, povertyID) %>%
  summarise(livestock_HH = round(sum(hhwt), 0))

pActivity_none_sn <- as.data.table(pActivity_none_sn)

pActivity_none_sn_cube <- cube(pActivity_none_sn, j = round(sum(livestock_HH), 2), by = c("strataID", "year", "rururbCode", "sexID", "AGE", "povertyID"), id = FALSE )

pActivity_none_sn_cube <- pActivity_none_sn_cube %>%
  filter(!is.na(strataID))

pActivity_none_sn_cube <- pActivity_none_sn_cube %>%
  filter(!is.na(year)) %>%
  rename(livestock_hh = V1)

pActivity_none_sn_cube <- pActivity_none_sn_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_none_sn_cube_DT <- pActivity_none_sn_cube %>%
  rename(GEO_PICT=strataID, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = livestock_hh, SEX = sexID, POVERTY = povertyID) %>%
  mutate(FREQ = "A", INDICATOR = "NHH", FOOD_ACTIVITY = "NONE", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")


pActivity_households_none_final <- rbind(pActivity_none_cube_DT, pActivity_none_sn_cube_DT)


#### ************************ Final combination of the tables ***************************** ####

combine_final <- rbind(
  primary_activity_households_final, 
  primary_activity_households_only_final,
  pActivity_households_none_final
)

#### *********************** proportions tabulation section ********************************** ####

# Number of of households by geographical locations
# National number of households

pActivity_geo_households <- pActivity %>%
  group_by(countryCode, year, rururbCode) %>%
  summarise(households = round(sum(hhwt), 0))

pActivity_geo_households <- as.data.table(pActivity_geo_households)

pActivity_geo_households <- pActivity_geo_households |>
  rename(GEO_PICT = countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode)

pActivity_geo_households$TIME_PERIOD <- as.character(pActivity_geo_households$TIME_PERIOD)

# Strata number of households

pActivity_geo_households_strata <- pActivity %>%
  group_by(strataID, year, rururbCode) %>%
  summarise(households = round(sum(hhwt), 0))

pActivity_geo_households_strata <- as.data.table(pActivity_geo_households_strata)

pActivity_geo_households_strata <- pActivity_geo_households_strata |>
  rename(GEO_PICT = strataID, TIME_PERIOD = year, URBANIZATION = rururbCode)

pActivity_geo_households_strata$TIME_PERIOD <- as.character(pActivity_geo_households_strata$TIME_PERIOD)
pActivity_geo_households_combine <- rbind(pActivity_geo_households, pActivity_geo_households_strata)

pActivity_geo_households_combine_cube <- cube(pActivity_geo_households_combine, j = round(sum(households), 0), by = c("GEO_PICT", "TIME_PERIOD", "URBANIZATION"), id = FALSE )

pActivity_geo_households_combine_cube <- pActivity_geo_households_combine_cube %>%
  filter(!is.na(GEO_PICT))

pActivity_geo_households_combine_cube <- pActivity_geo_households_combine_cube |>
  filter(!is.na(TIME_PERIOD))

pActivity_geo_households_combine_cube <- pActivity_geo_households_combine_cube %>%
  mutate_all(~replace(., is.na(.), "_T"))

#Merge household counts 
pActivity_primary_activity_percent <- merge(combine_final, pActivity_geo_households_combine_cube, by = c("GEO_PICT", "TIME_PERIOD", "URBANIZATION"), all = TRUE)

pActivity_primary_activity_percent$OBS_VALUE <- as.numeric(pActivity_primary_activity_percent$OBS_VALUE)
pActivity_primary_activity_percent$V1 <- as.numeric(pActivity_primary_activity_percent$V1)

#Calculate the percentages
pActivity_primary_activity_percent$percent <- round(pActivity_primary_activity_percent$OBS_VALUE/pActivity_primary_activity_percent$V1 * 100, 0)

#Finalising the percentage table
pActivity_primary_activity_percent <- pActivity_primary_activity_percent |>
  select(-V1, -OBS_VALUE) |>
  rename(OBS_VALUE = percent) |>
  mutate(INDICATOR = "PRPHH",
         UNIT_MEASURE = "PERCENT"
         )

#overall combination of household numbers of percentages

household_primary_activity_overall <- rbind(combine_final, pActivity_primary_activity_percent)
household_primary_activity_overall <- household_primary_activity_overall |>
  mutate(DATAFLOW = "SPC:DF_FOOD_SECURITY_HIES_2(1.0)",
         FOOD_SECURITY = "_T",
         DATA_SOURCE = paste0("Household Income and Expenditure Survey (HIES)",TIME_PERIOD)
         )

#Re-order the columns in the proper order

household_primary_activity_overall <- household_primary_activity_overall |>
  select(DATAFLOW,
         FREQ,
         TIME_PERIOD,
         GEO_PICT,
         INDICATOR,
         SEX,
         AGE,
         URBANIZATION,
         POVERTY,
         FOOD_SECURITY,
         FOOD_ACTIVITY,
         OBS_VALUE,
         UNIT_MEASURE,
         UNIT_MULT,
         OBS_STATUS,
         DATA_SOURCE,
         OBS_COMMENT,
         CONF_STATUS
         )

household_primary_activity_overall$OBS_VALUE <- as.numeric(household_primary_activity_overall$OBS_VALUE)

#Filter out records with null obs_value and select distinct records
household_primary_activity_overall <- household_primary_activity_overall |>
  filter(!is.na(OBS_VALUE)) |>
  distinct()


#Write the final table to the output folder
write.csv(household_primary_activity_overall, "output/household_primary_activity_overall.csv", row.names = FALSE)
