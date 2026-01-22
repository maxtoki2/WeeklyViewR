# sport in tv
print("sport")

# load all the parsers
source("tv_sports/nhl_functions.R")

# aggreaga
tv_sports <- bind_rows(
  prepare_nhl_table(get_nhl_games())
)

# restituisci tabella
tv_sports