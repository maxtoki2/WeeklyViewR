# sky sport
pull_sky_json <- function(
    day_start = today()
    , day_end = today()
    , url = glue("https://apid.sky.it/gtv/v1/events?v=1.0.89-prod&to={day_end}T23%3A59%3A59Z&pageSize=0&pageNum=0&from={day_start}T00%3A00%3A00Z&env=DTH&subGenres=&channelCategories=6")
){
  res <- GET(url)
  json_text <- content(res, "text", encoding = "UTF-8")
  
}

# sky_json <- pull_sky_json()

parse_sky_json <- function(j){
  dat <- fromJSON(j, flatten = TRUE)[[1]]
  
  if(class(dat) == "character"){
    NULL
  } else {
    dat %>%
      filter(str_detect(channel.name, "Sky Sport")) %>% 
      filter(programHighlights == "L" & content.episodeNumber > 0 & !str_detect(content.contentTitle, "News") & is.na(ppvReferenceId)) %>% # I think this gets live and excludes talk shows/news
      distinct() 
  }
}

prepare_sky_table <- function(
    parsed_games
    # TODO: hours to show, select sport / team / etc
    # keywords to include/exclude (eventTitle, eventSynopsis, contenTitle)
    # parsed_games %>% filter(if_any(contains(c("Title", "Synopsis")), ~str_detect(.x, "Test")))
    , keywords = sky_parameters$keywords
    , valid_hours = sky_parameters$valid_hours
){
  parsed_games %>% 
    inner_join(keywords, c("content.subgenre.name" = "Sport")) %>% 
    mutate(kw_regex_include = coalesce(purrr::map_chr(kw_include, ~ str_c(.x, collapse = "|")), "")) %>% 
    mutate(kw_regex_exclude = coalesce(purrr::map_chr(kw_exclude, ~ str_c(.x, collapse = "|")), "")) %>% 
    filter(if_any(contains(c("Title", "Synopsis")), ~str_detect(tolower(.x), tolower(kw_regex_include)))) %>% 
    filter(if_all(contains(c("Title", "Synopsis")), ~str_detect(tolower(.x), tolower(kw_regex_exclude), negate = TRUE))) %>% 
    mutate(starttime = ymd_hms(starttime, tz = "Europe/Rome")) %>% 
    filter(hour(starttime) %in% valid_hours) %>% 
    mutate(
      data = date(starttime)
      , ora = glue("{hour(starttime)}:{sprintf('%02d', minute(starttime))}")
      , descrizione = glue("{ora} SKY {eventTitle}")
      , gruppo = "tv_sports"
      , testo_immagine = descrizione
      , immagine = as.character(glue("tv_sports/glyphs/{tolower(glyph)}"))
      , colore = NA_character_
    ) 
}

# parsed_games <- parse_sky_json(pull_sky_json())
