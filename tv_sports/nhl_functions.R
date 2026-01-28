# NHL functions

pull_nhl_json <- function(date){
  url <- paste0("https://api-web.nhle.com/v1/schedule/", date)
  
  res <- httr::GET(url)
  httr::stop_for_status(res)
  
  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(txt, simplifyVector = TRUE)
}

# pull_nhl_json(today())

parse_nhl_json <- function(j){
  lapply(1:length(j$gameWeek$date), function(i){
    games_df <- j$gameWeek[i,"games"][[1]] 
    if(nrow(games_df) > 0){
      games_df %>% 
        unnest(cols = c(venue, tvBroadcasts, awayTeam, homeTeam, periodDescriptor), names_sep = "_") %>% 
        select(startTimeUTC, awayTeam_abbrev, homeTeam_abbrev)
    } else {
      games_df
    }
    
  }) %>% 
    bind_rows() %>%
    distinct() %>% 
    mutate(
      time_utc = ymd_hms(startTimeUTC, tz = "UTC"),
      time_cet = with_tz(time_utc, "Europe/Zurich")
    )
}

# parse_nhl_json(pull_nhl_json(today()))

get_nhl_games <- function(dates_list = periodo){
  dates_vector <- dates_list
  all_games <- NULL
  while(length(dates_vector) > 0){
    batch_json <- pull_nhl_json(dates_vector[1])  
    batch_games <- parse_nhl_json(batch_json)
    all_games <- bind_rows(all_games, batch_games)
    batch_next <- batch_json$nextStartDate
    dates_vector <- dates_vector[which(dates_vector >= batch_next)]
  }
  all_games
}
# get_nhl_games()

# TODO: parse
prepare_nhl_table <- function(parsed_games, hours_to_show = 6:23){
  parsed_games %>% 
    filter(hour(time_cet) %in% hours_to_show) %>% 
    mutate(
      data = date(time_cet)
      , ora = glue("{hour(time_cet)}:{sprintf('%02d', minute(time_cet))}")
      , descrizione = glue("{ora} NHL {awayTeam_abbrev} @ {homeTeam_abbrev}")
      , gruppo = "tv_sports"
      , testo_immagine = descrizione
      , immagine = "tv_sports//glyphs//hockey.png"
      , colore = NA_character_
    )
}

