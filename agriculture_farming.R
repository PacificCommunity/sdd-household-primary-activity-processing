#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Load setup thus running the required libraries and create some subsidiary tables

source("src/setup.R")

#### ********************************* Fishing method processing ******************************* ####

#process the country table
agriculture <- pActivity

agriculture <- agriculture |>
  filter(agric == 1) |>
  group_by(countryCode, year, rururbCode, sexID, AGE, agric_vege, agric_tuber, agric_fruit) |>
  summarise(totHH = round(sum(hhwt),0))

agriculture <- agriculture |>
  mutate(agric_vege = ifelse(is.na(agric_vege), "No", "Yes"),
         agric_tuber = ifelse(is.na(agric_tuber), "No", "Yes"),
         agric_fruit = ifelse(is.na(agric_fruit), "No", "Year")
  )

#convert table to datatable format

agriculture <- as.data.table(agriculture)

agriculture_cube <- cube(agriculture, j = round(sum(totHH), 2), by = c("countryCode", "year", "rururbCode", "sexID", "AGE", "agric_vege", "agric_tuber", "agric_fruit"), id = FALSE )

agriculture_cube <- agriculture_cube %>%
  filter(!is.na(countryCode))

agriculture_cube <- agriculture_cube %>%
  filter(!is.na(year)) %>%
  rename(households = V1)

agriculture_cube <- agriculture_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N") |>
  rename(GEO_PICT = countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, SEX = sexID,
         AGRICULTURE_VEGETABLE = agric_vege, AGRICULTURE_TUBER = agric_tuber, AGRICULTURE_FRUIT = agric_fruit
  )


#### ************************** Process Fishing method by household number ******************************************** ####

#Process the strata tabale

agriculture_str <- pActivity

agriculture_str <- agriculture_str |>
  filter(agric == 1) |>
  group_by(strataID, year, rururbCode, sexID, AGE, agric_vege, agric_tuber, agric_fruit) |>
  summarise(totHH = round(sum(hhwt),0))

agriculture_str <- agriculture_str |>
  mutate(agric_vege = ifelse(is.na(agric_vege), "No", "Yes"),
         agric_tuber = ifelse(is.na(agric_tuber), "No", "Yes"),
         agric_fruit = ifelse(is.na(agric_fruit), "No", "Year")
  )

#convert table to datatable format

agriculture_str <- as.data.table(agriculture_str)

agriculture_str_cube <- cube(agriculture_str, j = round(sum(totHH), 2), by = c("strataID", "year", "rururbCode", "sexID", "AGE", "agric_vege", "agric_tuber", "agric_fruit"), id = FALSE )

agriculture_str_cube <- agriculture_str_cube %>%
  filter(!is.na(strataID))

agriculture_str_cube <- agriculture_str_cube %>%
  filter(!is.na(year)) %>%
  rename(households = V1)

agriculture_str_cube <- agriculture_str_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N") |>
  rename(GEO_PICT = strataID, TIME_PERIOD = year, URBANIZATION = rururbCode, SEX = sexID,
         AGRICULTURE_VEGETABLE = agric_vege, AGRICULTURE_TUBER = agric_tuber, AGRICULTURE_FRUIT = agric_fruit)

#merge both the country and strata tables
agriculture_combine <- rbind(agriculture_cube, agriculture_str_cube)


agriculture_combine_DT <- agriculture_combine %>%
  rename(OBS_VALUE = households) |>
  mutate(FREQ = "A", INDICATOR = "NHH", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Re-organise the columns in the proper order

agriculture_combine_DT <- agriculture_combine_DT |>
  select(FREQ, TIME_PERIOD, GEO_PICT, URBANIZATION, SEX, AGE, AGRICULTURE_VEGETABLE, AGRICULTURE_TUBER, AGRICULTURE_FRUIT, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, CONF_STATUS)

#Write the results to output folder in a csv format

write.csv(agriculture_combine_DT, "output/agriculture_farming.csv", row.names = FALSE)
