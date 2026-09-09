# sport in tv
print("sport")

# load all the parsers
# TODO: make it a lapply
source("tv_sports/nhl_functions.R")
source("tv_sports/sky_functions.R")
source("tv_sports/rsi_functions.R")
source("tv_sports/ct_functions.R")
source("tv_sports/dazn_functions.R")

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

message("\nNHL")
nhl_tbl <- prepare_nhl_table(get_nhl_games())
message("\nSKY")
sky_tbl <- sky_schedule %>% select(data, ora, descrizione, gruppo, testo_immagine, immagine, colore) %>% distinct()
message("\nRSI")
rsi_tbl <- parse_rsi()
message("\nCT")
ct_tbl <- prepare_ct_table()
message("\nDAZN")
dazn_tbl <- prepare_dazn_table()

# aggreaga
tv_sports <- bind_rows(
  # TODO: make them all consistent (prepare_[xyz]_table)
  nhl_tbl
  , sky_tbl
  , rsi_tbl
  , ct_tbl
  , dazn_tbl
) %>% 
  arrange(data, ora)

# restituisci tabella
tv_sports
