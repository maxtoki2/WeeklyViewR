cat("preparation")

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
library(rvest)

source("lifeR_helpers.R")

param_giorni <- 14

param_url_mensa_base <- "https://www.comune.sassomarconi.bologna.it/myportal/C_G972/api/content/download"
param_url_mensa_doc_id <- "68f5e18203ce61009b6a9f32"

param_url_mensa <- glue("{param_url_mensa_base}?id={param_url_mensa_doc_id}")

param_id_medici <- c("boschi" = 11047, "piccolantonio" = 12418,"alberghini" = 16841)

param_info <- tribble(
  ~gruppo, ~tabella, ~riga1, ~weekend_offset, ~font_size
  , "rusco", "generale", 10, 8, 10
  , "calendario", "generale", 1, 0, 6 # DEBUG: pulldata assegna "famiglia"
  , "mlb", "generale", 13, 9, 10
  , "mensa", "mensa", 4, 0, 8
  , "medici", "medici", 2, -3, 8
  , "guardians", "guardians", 2, 0, 10
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
lista_dati <- list()
for(i in 1:nrow(param_info)){
  topic <- param_info$gruppo[i]
  # TODO: sposta print() qui
  
  dati_pulled <- source(glue("{topic}/{topic}_pulldata.R"), local = TRUE)$value
  
  if(!is.null(dati_pulled) && nrow(dati_pulled) > 0){
    dati_el <- dati_pulled %>%
      mutate(gruppo = ifelse(gruppo == "famiglia", "calendario", gruppo)) %>% # TODO: address elsewhere
      assign_cell()   
  } else {
    dati_el <- NULL
  }
  
  lista_dati <- c(lista_dati, list(dati_el))
}

dati <- bind_rows(lista_dati)

tabelle <- unique(param_info$tabella)

lapply(1:2, function(w){
  # print(w)
  week_tabs <- lapply(tabelle, function(tab){
    # print(tab)
    subdati <- dati %>% filter(tabella == tab) 
    if(nrow(subdati %>% filter(pagina == w)) > 0){
      subdati %>% table_prepare(w)
    } else {
      data.frame(`Non Disponibile` = character(0)) %>% flextable() 
    }  
  })
  names(week_tabs) <- tabelle
  week_tabs
}) -> tabelle_formattate

rmarkdown::render("index.Rmd")
