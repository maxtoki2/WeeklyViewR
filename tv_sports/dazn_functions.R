# DAZN functions
pull_dazn_data <- function(date = today(), baseurl = glue("https://epg.discovery.indazn.com/eu/v2/Epg?%24format=json&date={date}&country=IT&languageCode=it")){
  res <- GET(baseurl)
  if(!http_error(res))
    fromJSON(baseurl)$Tiles
}

# parsed_dazn <- pull_dazn_data()


parse_dazn_data <- function(parsed_dazn){
  parsed_dazn %>% 
    filter(str_detect(EntitlementIds, str_c(dazn_parameters$subscriptions, collapse = "|"))) %>% 
    filter(!str_detect(tolower(Title), str_c(lista_esclusione, collapse = "|"))) %>%  #filter on non linear?
    select(EventStartTime, Label, Title, Sport) %>% 
    rename(EventTitle = Title) %>% 
    unnest(Sport) %>% 
    mutate(
      data = ymd(substr(EventStartTime, 1, 10))
      , ora = substr(EventStartTime, 12, 16)
    ) %>% 
    inner_join(dazn_parameters$sport_mapping, by = "Title") %>%  # TODO: left and coalesce
    mutate(
      descrizione = glue("{ora} DZN {EventTitle}") # TODO: translate? Add descr?
      , gruppo = "tv_sports"
      , testo_immagine = descrizione
      , immagine = glue("tv_sports/glyphs/{tolower(Sport)}.png")
      , colore = NA
    )
}

prepare_dazn_table <- function(dates_list = periodo){
  lapply(dates_list, function(dt){
    parse_dazn_data(pull_dazn_data(dt)) #%>% bind_cols(date = dt)
  }) %>% 
    bind_rows() 
}
