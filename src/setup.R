#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Load setup thus running the required libraries and create some subsidiary tables

#Load data file
pActivity <- read_dta("T:/rara/RAP/Agriculture/sdd-household-primary-activity-processing/raw_data/SPC_REG-12_2012-2021_PRIMARY-ACTIVITIES.dta") %>%
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
  ),
  rururbCode = case_when(rururb == "urban" ~ "U",
                          rururb == "rural" ~ "R",
                          rururb == "national" ~ "N"
                          
  
  )
)

#Rural urban subtable
rururb <- data.frame(
  rururbCode = c(1,2,3,99),
  rururb = c("Rural", "Urban", "National", "Not Stated")
)

#Country subtable
country <- data.frame(
  countryCode = c("AS",	"CK",	"FJ",	"FM",	"GU",	"KI",	"MH",	"MP",	"NC",	"NR",	
                "NU",	"PF",	"PG",	"PN",	"PW",	"SB",	"TK",	"TO",	"TV",	"VU",	"WF",	"WS"),
  country = c("American Samoa",
                  "Cook Islands",
                  "Fiji",	
                  "Micronesia (Federated States of)",
                  "Guam",
                  "Kiribati",
                  "Marshall Islands",
                  "Northern Mariana Islands",
                  "New Caledonia",
                  "Nauru",
                  "Niue",
                  "French Polynesia",
                  "Papua New Guinea",
                  "Pitcairn",
                  "Palau",
                  "Solomon Islands",
                  "Tokelau",
                  "Tonga",
                  "Tuvalu",
                  "Vanuatu",
                  "Wallis and Futuna",
                  "Samoa"
)

)

pActivity <- merge(pActivity, country, by = "country")

