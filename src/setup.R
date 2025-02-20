#Load libraries
library(readxl)
library(openxlsx)
library(dplyr)
library(officer)
library(lubridate)
library(ggplot2)
library(haven)
library(data.table)
library(RSQLite)

#Create subsitiary dataframes
rururb <- data.frame(
  rururbID = c(1, 2, 3),
  rururbCode = c("U", "R", "N"),
  rururb = c("urban", "rural", "national")
)

country <- data.frame(
  countryCode = c("CK", "FM", "FJ", "KI", "MH", "NR", "NU", "PW", "PG", "WS", "SB", "TK", "TO", "TV", "VU", "WF"), 
  country = c("Cook Islands", "Fed. States of Micronesia", "Fiji", "Kiribati", "Marshall Islands", "Nauru", "Niue", "Palau", "Papua New Guinea",  "Samoa", "Solomon Islands", "Tokelau", "Tonga", "Tuvalu", "Vanuatu", "Wallis & Futuna")
)

strata <- data.frame(
  strataID = c("KI-1", "KI-2", "KI-3", "KI-4", "KI-5",
               "MH-1", "MH-2", "MH-3", "MH-4", "MH-5", "MH-6",
               "TO-1", "TO-2", "TO-3", "TO-4", "TO-5", "TO-6",
               "VU-1", "VU-2", "VU-3", "VU-4", "VU-5", "VU-6", "VU-7", "VU-8",
               "WF-1", "WF-2"
               
  ),
  strataName = c("South Tarawa", "Northern", "Central", "Southern", "Line Islands and Phoenix",
                 "Urban 1", "Urban 2", "Rural 1", "Rural 2", "Rural 3", "Rural 4",
                 "Tongatapu urban", "Tongatapu rural", "Vava'u", "Ha'apai", "Eua", "Ongo Niua",
                 "Torba - rural", "Sanma - rural", "Penama - rural", "Malampa - rural", "Shefa - rural", "Tafea - rural", "Sanma - urban", "Shefa - urban",
                 "Wallis", "Futuna"
  ),
  country = c("Kiribati", "Kiribati", "Kiribati", "Kiribati", "Kiribati",
              "Marshall Islands", "Marshall Islands", "Marshall Islands", "Marshall Islands", "Marshall Islands", "Marshall Islands",
              "Tonga", "Tonga", "Tonga", "Tonga", "Tonga", "Tonga",
              "Vanuatu", "Vanuatu", "Vanuatu", "Vanuatu", "Vanuatu", "Vanuatu", "Vanuatu", "Vanuatu",
              "Wallis and Futuna", "Wallis and Futuna"
  )
)


