#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Load setup thus running the required libraries and create some subsidiary tables

source("src/setup.R")

#### ********************************* Fishing method processing ******************************* ####

#process the country table
fishing <- pActivity

fishing <- fishing |>
  #filter(fisheries == 1 & countryCode != "VU") |>
  group_by(countryCode, year, rururbCode, sexID, AGE, fishloc_inshore, fishloc_nearshore, fishloc_offshore, fishloc_other) |>
  summarise(totHH = round(sum(hhwt),0))

fishing <- fishing |>
  mutate(fishloc_inshore = ifelse(is.na(fishloc_inshore), "No", "Yes"),
         fishloc_nearshore = ifelse(is.na(fishloc_nearshore), "No", "Yes"),
         fishloc_offshore = ifelse(is.na(fishloc_offshore), "No", "Yes"),
         fishloc_other = ifelse(is.na(fishloc_other), "Yes", "No")
  )

#convert table to datatable format

fishing <- as.data.table(fishing)

fishing_cube <- cube(fishing, j = round(sum(totHH), 2), by = c("countryCode", "year", "rururbCode", "sexID", "AGE", "fishloc_inshore", "fishloc_nearshore", "fishloc_offshore", "fishloc_other"), id = FALSE )

fishing_cube <- fishing_cube %>%
  filter(!is.na(countryCode))

fishing_cube <- fishing_cube %>%
  filter(!is.na(year)) %>%
  rename(households = V1)

fishing_cube <- fishing_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N") |>
  rename(GEO_PICT = countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, SEX = sexID,
         FISHING_INSHORE = fishloc_inshore, FISHING_NEARSHORE = fishloc_nearshore, FISHING_OFFSHORE = fishloc_offshore, FISHING_OTHER_LOCATION = fishloc_other
  )


#Process the strata tabale

fishing_str <- pActivity

fishing_str <- fishing_str |>
  #filter(fisheries == 1 & countryCode != "VU") |>
  group_by(strataID, year, rururbCode, sexID, AGE, fishloc_inshore, fishloc_nearshore, fishloc_offshore, fishloc_other) |>
  summarise(totHH = round(sum(hhwt),0))

fishing_str <- fishing_str |>
  mutate(fishloc_inshore = ifelse(is.na(fishloc_inshore), "No", "Yes"),
         fishloc_nearshore = ifelse(is.na(fishloc_nearshore), "No", "Yes"),
         fishloc_offshore = ifelse(is.na(fishloc_offshore), "No", "Yes"),
         fishloc_other = ifelse(is.na(fishloc_other), "Yes", "No")
  )

#convert table to datatable format

fishing_str <- as.data.table(fishing_str)

fishing_str_cube <- cube(fishing_str, j = round(sum(totHH), 2), by = c("strataID", "year", "rururbCode", "sexID", "AGE", "fishloc_inshore", "fishloc_nearshore", "fishloc_offshore", "fishloc_other"), id = FALSE )

fishing_str_cube <- fishing_str_cube %>%
  filter(!is.na(strataID))

fishing_str_cube <- fishing_str_cube %>%
  filter(!is.na(year)) %>%
  rename(households = V1)

fishing_str_cube <- fishing_str_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N") |>
  rename(GEO_PICT = strataID, TIME_PERIOD = year, URBANIZATION = rururbCode, SEX = sexID,
         FISHING_INSHORE = fishloc_inshore, FISHING_NEARSHORE = fishloc_nearshore, FISHING_OFFSHORE = fishloc_offshore, FISHING_OTHER_LOCATION = fishloc_other
  )

#merge both the country and strata tables
fishing_combine <- rbind(fishing_cube, fishing_str_cube)


fishing_combine_DT <- fishing_combine %>%
  rename(OBS_VALUE = households) |>
  mutate(FREQ = "A", INDICATOR = "N", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Re-organise the columns in the proper order

fishing_combine_DT <- fishing_combine_DT |>
  select(FREQ, TIME_PERIOD, GEO_PICT, URBANIZATION, INDICATOR, SEX, AGE, FISHING_INSHORE, FISHING_NEARSHORE, FISHING_OFFSHORE, FISHING_OTHER_LOCATION, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, CONF_STATUS)

#### **************************** Calculate Percentage ************************************* ####

source("households.R") #Get the number of households from the script 'households.R'

combine_hh <- households(hhNum)


fishing_location_percentage <- merge(fishing_combine_DT, combine_hh, by = c("FREQ", "TIME_PERIOD", "GEO_PICT", "URBANIZATION", "SEX", "AGE"))
fishing_location_percentage$percentage <- round(as.numeric(fishing_location_percentage$OBS_VALUE)/as.numeric(fishing_location_percentage$totHH) * 100, 0)

#Rename percentage to OBS_VALUE

fishing_location_percentage <- fishing_location_percentage |>
  select(-OBS_VALUE, -totHH) |>
  rename(OBS_VALUE = percentage) |>
  mutate(INDICATOR = "PERCENT",
         UNIT_MEASURE = "PERCENT"
         )

#Combine count table and percent table

fishing_locaion_final <- rbind(fishing_combine_DT, fishing_location_percentage)

fishing_locaion_final <- fishing_locaion_final |>
  mutate(DATAFLOW = "") |>
  select(DATAFLOW, everything()) |>
  distinct()

#Write the final fishing location table to csv file

write.csv(fishing_locaion_final, "output/fisheries/fishing_location_final.csv", row.names = FALSE)


#### ************************* Generate metadata ************************************** ####

fishing_locaion_final_metadata <- fishing_locaion_final 
fishing_locaion_final_metadata$GEO <- substr(fishing_locaion_final_metadata$GEO_PICT, 1, 2) 

fishing_locaion_final_metadata <- fishing_locaion_final_metadata |>
  group_by(FREQ, TIME_PERIOD, GEO) |>
  summarise(totcnt = n()
  )

fishing_locaion_final_metadata <- fishing_locaion_final_metadata |>
  rename(GEO_PICT = GEO) |>
  mutate(STRUCTURE = "dataflow",
         STRUCTURE_ID = "SPC:DF_AGRICULTURE_HIES(1.0)",
         ACTION = "R",
         INDICATOR = "~",
         URBANIZATION = "~",
         SEX = "~",
         AGE = "~",
         LIVESTOCK_PIG = "~",
         LIVESTOCK_CHICKEN = "~",
         LIVESTOCK_DUCK = "~",
         LIVESTOCK_OTHER = "~",
         DATA_SOURCE.DATA_SOURCE_ORGANIZATION = "",
         DATA_SOURCE.DATA_SOURCE_TITLE = "Household Income and Expenditure Survey",
         DATA_SOURCE.DATA_SOURCE_LICENSE = "",
         DATA_SOURCE.DATA_SOURCE_DATE = TIME_PERIOD,
         DATA_SOURCE.DATA_SOURCE_LINK = "",
         DATA_SOURCE.DATA_SOURCE_COMMENT = "",
         DATA_PROCESSING = "",
         DATA_REVISION = "",
         DATA_COMMENT = ""
         
  ) 

#Write the metadata table to a csv output file
write.csv(fishing_locaion_final_metadata, "output/fisheries/fishing_locaion_final_metadata.csv", row.names = FALSE)



