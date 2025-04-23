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

source("lifeR_helpers.R")

param_giorni <- 14
param_url_mensa <- "http://www2.comune.sassomarconi.bologna.it/upload/sassomarconi_ecm8v2/gestionedocumentale/ScuoledellInfanzia_784_16225.pdf"
param_inzio_periodo_mensa <- "21-10-2024" # TODO: possibly infer from file
param_fine_periodo_mensa <- "19-04-2025" # TODO: possibly infer from file

secr_calendario_famiglia <- Sys.getenv("GCAL_FAMIGLIA")
secr_id_indirizzo <- Sys.getenv("ID_INDIRIZZO")
secr_id_civico <- Sys.getenv("ID_CIVICO")

param_calendari <- c(
    "famiglia" = glue("https://calendar.google.com/calendar/ical/qblk3l3l4vu78epu50r1k3jmj0%40group.calendar.google.com/private-{secr_calendario_famiglia}/basic.ics")
    # , "guardians" = "http://ics.ecal.com/ecal-sub/675056cc2c79a8000823d913/MLB.ics"
  )

param_celle <- tribble(
  ~gruppo, ~riga1, ~weekend_offset, ~font_size
  , "mensa", 10, 0, 8
  , "rusco", 16, 8, 10
  , "famiglia", 1, 0, 6
  , "mlb", 19, 9, 10
)

giorno <- today()
inizio_settimana <- ymd(floor_date(giorno, "week"))
periodo <- ymd(seq(inizio_settimana, by = "1 day", length.out = param_giorni))

# helpers
assign_cell <- . %>%
  group_by(data) %>% 
  mutate(col_group = row_number()) %>% 
  ungroup() %>% 
  inner_join(param_celle, by = "gruppo") %>% 
  mutate(
    pagina = isoweek(data) - isoweek(inizio_settimana) + 1
    , colonna = wday(data, week_start = 1)
    , riga = riga1 - 1 + col_group
  ) %>% 
  mutate(
    riga = pmax(1, riga - ifelse(wday(data) > 5, weekend_offset, 0))
  ) %>% 
  select(any_of(c("pagina", "riga", "colonna", "descrizione", "gruppo", "immagine", "colore", "testo_immagine", names(param_celle)[4:ncol(param_celle)])))

#########
# rifiuti
#########
print("rifiuti")
source("rusco/rifiutologo.R")
baseurl_immagini_rusco <- "https://webapp-ambiente.gruppohera.it/assets/images/pictograms/"
filext_rusco <- "png"
folder_rusco <- "rusco/saved"
file_rusco <- glue("rusco{min(periodo)}_{max(periodo)}.RDS")
if(file_rusco %in% list.files(folder_rusco)){
  rusco_periodo <- readRDS(glue("{folder_rusco}/{file_rusco}"))  
} else {
  rusco_periodo <- get_calendar(inizio_settimana, param_giorni)
  saveRDS(rusco_periodo, glue("{folder_rusco}/{file_rusco}"))  
}
# TODO: formatta, combina immagini

# info periodo scelto
rusco <- rusco_periodo %>% 
  filter(data %in% periodo) %>% 
  mutate(gruppo = "rusco")

# scarica immagini
lapply(unique(rusco$nomeFile), function(x){
  get_images(x, filext_rusco, base_url = baseurl_immagini_rusco, save_folder = folder_rusco)
}) %>% 
  invisible()

rusco <- rusco %>% 
  #add_image_column(filename_column = "nomeFile", filext_rusco, save_folder = folder_rusco, base_url = baseurl_immagini_rusco) %>% 
  mutate(immagine = as.character(glue("{folder_rusco}/{nomeFile}.{filext_rusco}"))) %>% 
  assign_cell()
  

##############
# mensa scuola
##############
print("mensa")
source("mensa/mensa_scuola.R")
folder_mensa <- "mensa/saved"
file_mensa <- glue("mensa{param_inzio_periodo_mensa}_{param_fine_periodo_mensa}.RDS")
if(file_mensa %in% list.files(folder_mensa)){
  mensa_periodo <- readRDS(glue("{folder_mensa}/{file_mensa}"))  
} else {
  mensa_periodo <- get_menu(param_inzio_periodo_mensa, param_fine_periodo_mensa, param_url_mensa)
  saveRDS(mensa_periodo, glue("{folder_mensa}/{file_mensa}"))  
}
mensa <- mensa_periodo %>% 
  filter(data %in% periodo) %>% 
  mutate(gruppo = "mensa") %>% 
  assign_cell()

############
# calendario
############
print("calendario")
source("calendario/google_calendar.R")
calendario_completo <- get_calendars(param_calendari)
calendario <- calendario_completo %>% 
  filter(data %in% periodo) %>% 
  # group_by(data) %>% 
  # mutate(col_group = row_number()) %>% 
  # ungroup() %>% 
  assign_cell()

############
# mlb
############
print("mlb")
source("mlb/mlb.R")
baseurl_mlb_logo <- "https://www.mlbstatic.com/team-logos"
orig_filext_mlb <- "svg"
outp_filext_mlb <- "png"
folder_mlb <- "mlb/saved"
file_mlb <- glue("mlb{min(periodo)}_{max(periodo)}.RDS")
if(file_mlb %in% list.files(folder_mlb)){
  mlb_periodo <- readRDS(glue("{folder_mlb}/{file_mlb}"))  
} else {
  mlb_periodo <- get_mlb_schedule(inizio_settimana, param_giorni, 114)
  saveRDS(mlb_periodo, glue("{folder_mlb}/{file_mlb}"))  
}
# TODO: formatta, combina immagini

if(!is.null(mlb_periodo)){
  # info periodo scelto
  mlb <- mlb_periodo %>% 
    filter(data %in% periodo) %>% 
    mutate(gruppo = "mlb")
  
  # scarica immagini
  lapply(unique(mlb$opponent_id), function(x){
    get_images(x, orig_filext_mlb, base_url = baseurl_mlb_logo, save_folder = folder_mlb, save_to_extension = outp_filext_mlb)
  }) %>% 
    invisible()
  
  mlb <- mlb %>% 
    mutate(immagine = as.character(glue("{folder_mlb}/{opponent_id}.{outp_filext_mlb}"))) %>% 
    assign_cell()
} else {
  mlb <- NULL
}

print("rendering tabelle")  
# TODO: formatta
tutto <- bind_rows(
  rusco
  , mensa
  , calendario
  , mlb
)

tables <- lapply(1:2, function(w) table_prepare(tutto, w))


# TODO: knit
rmarkdown::render("index.Rmd")


