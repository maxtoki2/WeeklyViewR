# sport in tv
print("sport")

# load all the parsers
source("tv_sports/nhl_functions.R")
source("tv_sports/sky_functions.R")
source("tv_sports/rsi_functions.R")

# processa
sky_schedule <- lapply(periodo, function(d){
  # print(d)
  sky_json <- pull_sky_json(d, d)
  parsed_games <- parse_sky_json(sky_json)
  if(!is.null(parsed_games)){
    prepare_sky_table(parsed_games)  
  } else {
    NULL
  }
}) %>% bind_rows()

# aggreaga
tv_sports <- bind_rows(
  prepare_nhl_table(get_nhl_games())
  , sky_schedule %>% select(data, ora, descrizione, gruppo, testo_immagine, immagine, colore) %>% distinct()
  , parse_rsi()
) %>% 
  arrange(data, ora)

# restituisci tabella
tv_sports
