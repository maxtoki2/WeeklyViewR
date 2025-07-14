print("preparation")

options("lubridate.week.start" = 1)
Sys.setlocale("LC_ALL", "it_IT.UTF-8")

# master
library(magick)
library(tidyr)
library(dplyr)
library(lubridate)
library(glue)
library(stringr)
library(httr)
library(jsonlite)
library(calendar)
library(flextable)
library(DescTools)
library(httr)
library(rvest)

source("lifeR_helpers.R")

param_giorni <- 14

param_url_mensa <- "https://www.comune.sassomarconi.bologna.it/myportal/C_G972/api/content/download?id=67ed52fd87031500992ed860"
param_inzio_periodo_mensa <- "14-04-2025" # TODO: possibly infer from file
param_fine_periodo_mensa <- "29-08-2025" # TODO: possibly infer from file

param_id_medici <- c(11047,16724,12418)

param_info <- tribble(
  ~gruppo, ~tabella
  , "rusco", "main"
  , "calendario", "main" # DEBUG: pulldata assegna "famiglia"
  , "mlb", "main"
  , "mensa", "mensa"
  , "medici", "main"
)

secr_calendario_famiglia <- Sys.getenv("GCAL_FAMIGLIA")
secr_id_indirizzo <- Sys.getenv("ID_INDIRIZZO")
secr_id_civico <- Sys.getenv("ID_CIVICO")

# TODO: move to calendario?
param_calendari <- c(
  "famiglia" = glue("https://calendar.google.com/calendar/ical/qblk3l3l4vu78epu50r1k3jmj0%40group.calendar.google.com/private-{secr_calendario_famiglia}/basic.ics")
  # , "guardians" = "http://ics.ecal.com/ecal-sub/675056cc2c79a8000823d913/MLB.ics"
)

giorno <- today()
inizio_settimana <- ymd(floor_date(giorno, "week"))
periodo <- ymd(seq(inizio_settimana, by = "1 day", length.out = param_giorni))

# scarica dati
lista_dati <- lapply(1:nrow(param_info), function(i){
  topic <- param_info$gruppo[i]
  # TODO: sposta print() qui
  
  source(glue("{topic}/{topic}_pulldata.R"))
  get(topic)
})

dati <- bind_rows(lista_dati)
