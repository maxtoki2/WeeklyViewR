parse_rsi <- function(
  date_from = min(periodo)
  , date_to = max(periodo)
  , exclude_list = rsi_parameters$exclude_list
  , url = "https://1uf2cfvii9.execute-api.eu-central-1.amazonaws.com/events-by-date"
){
  # Query API
  resp <- request(url) |>
    req_url_query(
      fromDate = glue("{date_from}Z")
      , toDate   = glue("{date_to}Z")
    ) |>
    req_perform()
  
  # Parse JSON
  events <- resp |> resp_body_json(simplifyVector = TRUE)
  
  # Convert to tibble if nested
  events_df <- as_tibble(events)
  
  # Keep only sports entries (assuming column/category field exists)
  sports_df <- events_df %>%
    filter(streamtype == "sport") %>% 
    select(sport, dateTimeInfo, displayTitle, title) %>% 
    unnest(cols = c(sport, dateTimeInfo)) %>% 
    select(name, fullDateTime, displayTitle, title) %>% 
    filter(!tolower(name) %in% tolower(exclude_list)) %>% 
    mutate(
      start_time = ymd_hms(fullDateTime)
      , title = str_remove(title, glue("{name}: "))
      , data = date(start_time)
      , ora = str_trim(str_remove(start_time, as.character(data)))
      , descrizione = glue("{ora} RSI {title}")
      , gruppo = "tv_sports"
      , testo_immagine = descrizione
      , immagine = as.character(glue("tv_sports/glyphs/{name}.png"))
      , colore = NA_character_
    )
  
  sports_df
  
}
