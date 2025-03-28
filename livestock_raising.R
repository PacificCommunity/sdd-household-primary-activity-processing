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
  mutate(livestock_pig = ifelse(is.na(livestock_pig), "NO", "YES"),
         livestock_chicken = ifelse(is.na(livestock_chicken), "NO", "YES"),
         livestock_duck = ifelse(is.na(livestock_duck), "NO", "YES"),
         livestock_other = ifelse(is.na(livestock_other), "NO", "YES")
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
  mutate(livestock_pig = ifelse(is.na(livestock_pig), "NO", "YES"),
         livestock_chicken = ifelse(is.na(livestock_chicken), "NO", "YES"),
         livestock_duck = ifelse(is.na(livestock_duck), "NO", "YES"),
         livestock_other = ifelse(is.na(livestock_other), "NO", "YES")
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
  mutate(FREQ = "A", INDICATOR = "N", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Re-organise the columns in the proper order

livestock_combine_DT <- livestock_combine_DT |>
  select(FREQ, TIME_PERIOD, GEO_PICT, URBANIZATION, INDICATOR, SEX, AGE, LIVESTOCK_PIG, LIVESTOCK_CHICKEN, LIVESTOCK_DUCK, LIVESTOCK_OTHER, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, CONF_STATUS)


#### **************************** Calculate Percentage ************************************* ####

source("households.R") #Get the number of households from the script 'households.R'

combine_hh <- households(hhNum)

livestock_percentage <- merge(livestock_combine_DT, combine_hh, by = c("FREQ", "TIME_PERIOD", "GEO_PICT", "URBANIZATION", "SEX", "AGE"))
livestock_percentage$percentage <- round(as.numeric(livestock_percentage$OBS_VALUE)/as.numeric(livestock_percentage$totHH) * 100, 0)

#Rename percentage to OBS_VALUE

livestock_percentage <- livestock_percentage |>
  select(-OBS_VALUE, -totHH) |>
  rename(OBS_VALUE = percentage) |>
  mutate(INDICATOR = "PERCENT",
         UNIT_MEASURE = "PERCENT"
  )

#Combine count table and percent table

livestock_combine_DT$OBS_VALUE <- as.numeric(livestock_combine_DT$OBS_VALUE)
livestock_final <- rbind(livestock_combine_DT, livestock_percentage)

#Add the dataflow reference

livestock_final <- livestock_final |> 
  mutate(DATAFLOW = "SPC:DF_LIVESTOCK(1.0)",
         DATA_SOURCE = paste0("Household Income and Expenditure Survey (HIES)",TIME_PERIOD)
         )
livestock_final <- livestock_final |> select(DATAFLOW, everything())

#Remove duplicates
livestock_final<- livestock_final |> distinct()


#Write the final fishing location table to csv file
write.csv(livestock_final, "output/livestock/livestock_final_data.csv", row.names = FALSE)


#### ************************* Generate metadata ************************************** ####

livestock_final_metadata <- livestock_final 
livestock_final_metadata$GEO <- substr(livestock_final_metadata$GEO_PICT, 1, 2) 

livestock_final_metadata <- livestock_final_metadata |>
  group_by(FREQ, TIME_PERIOD, GEO) |>
  summarise(totcnt = n()
    )

livestock_final_metadata <- livestock_final_metadata |>
  rename(GEO_PICT = GEO) |>
  mutate(STRUCTURE = "DATAFLOW", STRUCTURE_ID = "SPC:DF_LIVESTOCK(1.0)", ACTION = "I", INDICATOR = "~",
         URBANIZATION = "~", SEX = "~", AGE = "~", LIVESTOCK_PIG = "~", LIVESTOCK_CHICKEN = "~", LIVESTOCK_DUCK = "~",
         LIVESTOCK_OTHER = "~", DATA_SOURCE.DATA_SOURCE_ORGANIZATION = "", DATA_SOURCE.DATA_SOURCE_TITLE = "Household Income and Expenditure Survey",
         DATA_SOURCE.DATA_SOURCE_LICENSE = "", DATA_SOURCE.DATA_SOURCE_DATE = TIME_PERIOD, DATA_SOURCE.DATA_SOURCE_LINK = "",
         DATA_SOURCE.DATA_SOURCE_COMMENT = "", DATA_PROCESSING = "", DATA_REVISION = "", DATA_COMMENT = ""
         
         )

#Reorder the fields in the proper order

livestock_final_metadata <- livestock_final_metadata |>
  select(STRUCTURE, STRUCTURE_ID, ACTION, FREQ, TIME_PERIOD, GEO_PICT, URBANIZATION, INDICATOR, SEX, AGE,
         LIVESTOCK_PIG, LIVESTOCK_CHICKEN, LIVESTOCK_DUCK, LIVESTOCK_OTHER, DATA_SOURCE.DATA_SOURCE_ORGANIZATION,
         DATA_SOURCE.DATA_SOURCE_TITLE, DATA_SOURCE.DATA_SOURCE_LICENSE, DATA_SOURCE.DATA_SOURCE_DATE, DATA_SOURCE.DATA_SOURCE_LINK,
         DATA_SOURCE.DATA_SOURCE_COMMENT, DATA_PROCESSING, DATA_REVISION, DATA_COMMENT
         )
  
#Write the metadata table to a csv output file
write.csv(livestock_final_metadata, "output/livestock/livestock_final_metadata.csv", row.names = FALSE)
