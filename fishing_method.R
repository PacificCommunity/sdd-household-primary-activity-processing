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
  mutate(fishmethod_gleaning = ifelse(is.na(fishmethod_gleaning), "No", "Yes"),
         fishmethod_line = ifelse(is.na(fishmethod_line), "No", "Yes"),
         fishmethod_net = ifelse(is.na(fishmethod_net), "No", "Year"),
         fishmethod_spear = ifelse(is.na(fishmethod_spear), "Yes", "No"),
         fishmethod_other = ifelse(is.na(fishmethod_other), "Yes", "No")
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
  mutate(fishmethod_gleaning = ifelse(is.na(fishmethod_gleaning), "No", "Yes"),
         fishmethod_line = ifelse(is.na(fishmethod_line), "No", "Yes"),
         fishmethod_net = ifelse(is.na(fishmethod_net), "No", "Year"),
         fishmethod_spear = ifelse(is.na(fishmethod_spear), "Yes", "No"),
         fishmethod_other = ifelse(is.na(fishmethod_other), "Yes", "No")
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
  mutate(FREQ = "A", INDICATOR = "NHH", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Re-organise the columns in the proper order

fishing_method_combine_DT <- fishing_method_combine_DT |>
  select(FREQ, TIME_PERIOD, GEO_PICT, URBANIZATION, SEX, AGE, FISHING_GLEANING, FISHING_LINE, FISHING_NET, FISHING_SPEAR, FISHING_OTHER_METHOD, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, CONF_STATUS)

#Write the results to output folder in a csv format

write.csv(fishing_method_combine_DT, "output/fishing_method.csv", row.names = FALSE)
