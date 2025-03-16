#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Load setup thus running the required libraries and create some subsidiary tables

source("src/setup.R")

#### ********************************* Fishing method processing ******************************* ####

#process the country table
livestock <- pActivity

livestock <- livestock |>
  filter(livestock == 1) |>
  group_by(countryCode, year, rururbCode, sexID, AGE, livestock_pig, livestock_chicken, livestock_duck, livestock_other) |>
  summarise(totHH = round(sum(hhwt),0))

livestock <- livestock |>
  mutate(livestock_pig = ifelse(is.na(livestock_pig), "No", "Yes"),
         livestock_chicken = ifelse(is.na(livestock_chicken), "No", "Yes"),
         livestock_duck = ifelse(is.na(livestock_duck), "No", "Year"),
         livestock_other = ifelse(is.na(livestock_other), "Yes", "No")
  )

#convert table to datatable format

livestock <- as.data.table(livestock)

livestock_cube <- cube(livestock, j = round(sum(totHH), 2), by = c("countryCode", "year", "rururbCode", "sexID", "AGE", "livestock_pig", "livestock_chicken", "livestock_duck", "livestock_other"), id = FALSE )

livestock_cube <- livestock_cube %>%
  filter(!is.na(countryCode))

livestock_cube <- livestock_cube %>%
  filter(!is.na(year)) %>%
  rename(households = V1)

livestock_cube <- livestock_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N") |>
  rename(GEO_PICT = countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, SEX = sexID,
         LIVESTOCK_PIG = livestock_pig, LIVESTOCK_CHICKEN = livestock_chicken, LIVESTOCK_DUCK = livestock_duck, LIVESTOCK_OTHER = livestock_other
  )


#Process the strata tabale

livestock_str <- pActivity

livestock_str <- livestock_str |>
  filter(livestock == 1) |>
  group_by(strataID, year, rururbCode, sexID, AGE, livestock_pig, livestock_chicken, livestock_duck, livestock_other) |>
  summarise(totHH = round(sum(hhwt),0))

livestock_str <- livestock_str |>
  mutate(livestock_pig = ifelse(is.na(livestock_pig), "No", "Yes"),
         livestock_chicken = ifelse(is.na(livestock_chicken), "No", "Yes"),
         livestock_duck = ifelse(is.na(livestock_duck), "No", "Year"),
         livestock_other = ifelse(is.na(livestock_other), "Yes", "No")
  )

#convert table to datatable format

livestock_str <- as.data.table(livestock_str)

livestock_str_cube <- cube(livestock_str, j = round(sum(totHH), 2), by = c("strataID", "year", "rururbCode", "sexID", "AGE", "livestock_pig", "livestock_chicken", "livestock_duck", "livestock_other"), id = FALSE )

livestock_str_cube <- livestock_str_cube %>%
  filter(!is.na(strataID))

livestock_str_cube <- livestock_str_cube %>%
  filter(!is.na(year)) %>%
  rename(households = V1)

livestock_str_cube <- livestock_str_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N") |>
  rename(GEO_PICT = strataID, TIME_PERIOD = year, URBANIZATION = rururbCode, SEX = sexID,
         LIVESTOCK_PIG = livestock_pig, LIVESTOCK_CHICKEN = livestock_chicken, LIVESTOCK_DUCK = livestock_duck, LIVESTOCK_OTHER = livestock_other
  )

#merge both the country and strata tables
livestock_combine <- rbind(livestock_cube, livestock_str_cube)


livestock_combine_DT <- livestock_combine %>%
  rename(OBS_VALUE = households) |>
  mutate(FREQ = "A", INDICATOR = "NHH", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Re-organise the columns in the proper order

livestock_combine_DT <- livestock_combine_DT |>
  select(FREQ, TIME_PERIOD, GEO_PICT, URBANIZATION, SEX, AGE, LIVESTOCK_PIG, LIVESTOCK_CHICKEN, LIVESTOCK_DUCK, LIVESTOCK_OTHER, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, CONF_STATUS)

#Write the results to output folder in a csv format

write.csv(livestock_combine_DT, "output/livestock_raising.csv", row.names = FALSE)
