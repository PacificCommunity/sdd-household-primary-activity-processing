#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Load setup thus running the required libraries and create some subsidiary tables

source("src/setup.R")

#### ********************************* Fishing method processing ******************************* ####

#process the country table
fishing_method <- pActivity

fishing_method <- fishing_method |>
  filter(fisheries == 1) |>
  group_by(countryCode, year, rururbCode, sexID, AGE, fishmethod_gleaning, fishmethod_line, fishmethod_net, fishmethod_spear, fishmethod_other) |>
  summarise(totHH = round(sum(hhwt),0))

fishing_method <- fishing_method |>
  mutate(fishmethod_gleaning = ifelse(is.na(fishmethod_gleaning), "NO", "YES"),
         fishmethod_line = ifelse(is.na(fishmethod_line), "NO", "YES"),
         fishmethod_net = ifelse(is.na(fishmethod_net), "NO", "YES"),
         fishmethod_spear = ifelse(is.na(fishmethod_spear), "NO", "YES"),
         fishmethod_other = ifelse(is.na(fishmethod_other), "NO", "YES")
  )

#convert table to datatable format

fishing_method <- as.data.table(fishing_method)

fishing_method_cube <- cube(fishing_method, j = round(sum(totHH), 2), by = c("countryCode", "year", "rururbCode", "sexID", "AGE", "fishmethod_gleaning", "fishmethod_line", "fishmethod_net", "fishmethod_spear", "fishmethod_other"), id = FALSE )

fishing_method_cube <- fishing_method_cube %>%
  filter(!is.na(countryCode))

fishing_method_cube <- fishing_method_cube %>%
  filter(!is.na(year)) %>%
  rename(households = V1)

fishing_method_cube <- fishing_method_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N") |>
  rename(GEO_PICT = countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, SEX = sexID,
         FISHING_GLEANING = fishmethod_gleaning, FISHING_LINE = fishmethod_line, FISHING_NET = fishmethod_net, FISHING_SPEAR = fishmethod_spear, FISHING_OTHER_METHOD = fishmethod_other
  )


#### ************************** Process Fishing method by household number ******************************************** ####

#Process the strata tabale

fishing_method_str <- pActivity

fishing_method_str <- fishing_method_str |>
  filter(fisheries == 1) |>
  group_by(strataID, year, rururbCode, sexID, AGE, fishmethod_gleaning, fishmethod_line, fishmethod_net, fishmethod_spear, fishmethod_other) |>
  summarise(totHH = round(sum(hhwt),0))

fishing_method_str <- fishing_method_str |>
  mutate(fishmethod_gleaning = ifelse(is.na(fishmethod_gleaning), "NO", "YES"),
         fishmethod_line = ifelse(is.na(fishmethod_line), "NO", "YES"),
         fishmethod_net = ifelse(is.na(fishmethod_net), "NO", "YES"),
         fishmethod_spear = ifelse(is.na(fishmethod_spear), "NO", "YES"),
         fishmethod_other = ifelse(is.na(fishmethod_other), "NO", "YES")
  )

#convert table to datatable format

fishing_method_str <- as.data.table(fishing_method_str)

fishing_method_str_cube <- cube(fishing_method_str, j = round(sum(totHH), 2), by = c("strataID", "year", "rururbCode", "sexID", "AGE", "fishmethod_gleaning", "fishmethod_line", "fishmethod_net", "fishmethod_spear", "fishmethod_other"), id = FALSE )

fishing_method_str_cube <- fishing_method_str_cube %>%
  filter(!is.na(strataID))

fishing_method_str_cube <- fishing_method_str_cube %>%
  filter(!is.na(year)) %>%
  rename(households = V1)

fishing_method_str_cube <- fishing_method_str_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N") |>
  rename(GEO_PICT = strataID, TIME_PERIOD = year, URBANIZATION = rururbCode, SEX = sexID,
         FISHING_GLEANING = fishmethod_gleaning, FISHING_LINE = fishmethod_line, FISHING_NET = fishmethod_net, FISHING_SPEAR = fishmethod_spear, FISHING_OTHER_METHOD = fishmethod_other
  )

#merge both the country and strata tables
fishing_method_combine <- rbind(fishing_method_cube, fishing_method_str_cube)

fishing_method_combine_DT <- fishing_method_combine %>%
  rename(OBS_VALUE = households) |>
  mutate(FREQ = "A", INDICATOR = "N", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Re-organise the columns in the proper order

fishing_method_combine_DT <- fishing_method_combine_DT |>
  select(FREQ, TIME_PERIOD, GEO_PICT, URBANIZATION, INDICATOR, SEX, AGE, FISHING_GLEANING, FISHING_LINE, FISHING_NET, FISHING_SPEAR, FISHING_OTHER_METHOD, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, CONF_STATUS)

#### **************************** Calculate Percentage ************************************* ####

source("households.R") #Get the number of households from the script 'households.R'

combine_hh <- households(hhNum)

fishing_method_percentage <- merge(fishing_method_combine_DT, combine_hh, by = c("FREQ", "TIME_PERIOD", "GEO_PICT", "URBANIZATION", "SEX", "AGE"))
fishing_method_percentage$percentage <- round(as.numeric(fishing_method_percentage$OBS_VALUE)/as.numeric(fishing_method_percentage$totHH) * 100, 0)

#Rename percentage to OBS_VALUE

fishing_method_percentage <- fishing_method_percentage |>
  select(-OBS_VALUE, -totHH) |>
  rename(OBS_VALUE = percentage) |>
  mutate(INDICATOR = "PERCENT",
         UNIT_MEASURE = "PERCENT"
  )

#Combine count table and percent table

fishing_method_final <- rbind(fishing_method_combine_DT, fishing_method_percentage)

fishing_method_final <- fishing_method_final |>
  mutate(DATAFLOW = "SPC:DF_FISHING_METHOD_HIES(1.0)",
         DATA_SOURCE = paste0("Household Income and Expenditure Survey (HIES)",TIME_PERIOD)
         ) |>
  select(DATAFLOW, everything()) |>
  distinct()

#Write the final fishing location table to csv file

write.csv(fishing_method_final, "output/fisheries/fishing_method_final.csv", row.names = FALSE)


#### ************************* Generate metadata ************************************** ####

fishing_method_final_metadata <- fishing_method_final 
fishing_method_final_metadata$GEO <- substr(fishing_method_final_metadata$GEO_PICT, 1, 2) 

fishing_method_final_metadata <- fishing_method_final_metadata |>
  group_by(FREQ, TIME_PERIOD, GEO) |>
  summarise(totcnt = n()
  )

fishing_method_final_metadata <- fishing_method_final_metadata |>
  rename(GEO_PICT = GEO) |>
  mutate(STRUCTURE = "DATAFLOW", STRUCTURE_ID = "SPC:DF_FISHING_METHOD_HIES(1.0)", ACTION = "I", INDICATOR = "~",
         URBANIZATION = "~", SEX = "~", AGE = "~", LIVESTOCK_PIG = "~", LIVESTOCK_CHICKEN = "~", LIVESTOCK_DUCK = "~",
         LIVESTOCK_OTHER = "~", DATA_SOURCE.DATA_SOURCE_ORGANIZATION = "", DATA_SOURCE.DATA_SOURCE_TITLE = "Household Income and Expenditure Survey",
         DATA_SOURCE.DATA_SOURCE_LICENSE = "", DATA_SOURCE.DATA_SOURCE_DATE = TIME_PERIOD, DATA_SOURCE.DATA_SOURCE_LINK = "",
         DATA_SOURCE.DATA_SOURCE_COMMENT = "", DATA_PROCESSING = "", DATA_REVISION = "",
         DATA_COMMENT = ""
         
  )

#Re-organise the fileds in proper order
fishing_method_final_metadata <- fishing_method_final_metadata |>
  select(STRUCTURE, STRUCTURE_ID, ACTION, FREQ, TIME_PERIOD, GEO_PICT, INDICATOR, URBANIZATION, SEX, AGE,
         LIVESTOCK_PIG, LIVESTOCK_CHICKEN, LIVESTOCK_DUCK, LIVESTOCK_OTHER, DATA_SOURCE.DATA_SOURCE_ORGANIZATION,
         DATA_SOURCE.DATA_SOURCE_TITLE, DATA_SOURCE.DATA_SOURCE_LICENSE, DATA_SOURCE.DATA_SOURCE_DATE, DATA_SOURCE.DATA_SOURCE_LINK,
         DATA_SOURCE.DATA_SOURCE_COMMENT, DATA_PROCESSING, DATA_REVISION, DATA_COMMENT
         )
  
#Write the metadata table to a csv output file
write.csv(fishing_method_final_metadata, "output/fisheries/fishing_method_final_metadata.csv", row.names = FALSE)
