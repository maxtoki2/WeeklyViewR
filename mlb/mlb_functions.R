# MLB schedule

get_mlb_schedule <- function(
    date_from = "2025-05-01"
    , n_days = 14
    , team_id = 114
    , base_url = "https://statsapi.mlb.com/api/v1/schedule?sportId=1&sportId=51&sportId=21&timeZone=America/New_York&gameType=E&&gameType=S&&gameType=R&&gameType=F&&gameType=D&&gameType=L&&gameType=W&&gameType=A&language=en&leagueId=104&&leagueId=103&&leagueId=160&&leagueId=590&&leagueId=&&leagueId="
    , timezone_display = "Europe/Berlin"
    , home_bg = "FFFFFF"
    , away_bg = "E5E5E5"
  ){
  #&startDate=2025-07-04&endDate=2025-07-06
  date_to <- ymd(date_from) + days(n_days)
  full_url <- glue("{base_url}&startDate={date_from}&endDate={date_to}")
  pulled_schedule <- fromJSON(full_url)
  games <- bind_rows(
    pulled_schedule$dates$games
  ) 
  if(nrow(games) > 0){
    games <- games %>% 
      unnest(teams) %>% 
      unnest(away, names_sep = "_") %>% 
      unnest(home, names_sep = "_") %>% 
      unnest(away_team, names_sep = "_") %>% 
      unnest(home_team, names_sep = "_") %>% 
      filter(home_team_id == team_id | away_team_id == team_id)
    
    parsed_games <- games %>% 
      mutate(data = ymd(substr(gameDate, 1, 10))) %>% 
      mutate(home_game = home_team_id == team_id) %>% 
      mutate(descrizione = ifelse(home_game, away_team_name, paste0("@ ", home_team_name))) %>% 
      mutate(opponent_id = ifelse(home_game, away_team_id, home_team_id)) %>% 
      mutate(
        datetime_parsed = ymd_hms(gameDate, tz = "US/Eastern"),  # Parse as US/Eastern time
        , orario = with_tz(datetime_parsed, tzone = timezone_display)  # Convert to Central Europe time
      ) %>% 
      arrange(orario) %>% 
      select(data, descrizione, home_game, orario, opponent_id) %>% 
      mutate(testo_immagine = format(orario, "%H:%M")) %>% 
      mutate(colore = ifelse(home_game, home_bg, away_bg))
  } else {
    parsed_games <- NULL
  }
  parsed_games
}
