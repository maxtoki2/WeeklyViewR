# sky sport
# https://apid.sky.it/gtv/v1/events?v=1.0.89-prod&to=2026-01-25T17%3A59%3A00Z&pageSize=25&pageNum=0&from=2026-01-23T12%3A00%3A00Z&env=DTH&subGenres=&channelCategories=6

library(httr)
library(jsonlite)

url <- "https://apid.sky.it/gtv/v1/events?v=1.0.89-prod&to=2026-01-25T17%3A59%3A00Z&pageSize=500&pageNum=0&from=2026-01-23T12%3A00%3A00Z&env=DTH&subGenres=&channelCategories=6"

res <- GET(url)
json_text <- content(res, "text", encoding = "UTF-8")

data <- fromJSON(json_text, flatten = TRUE)

data[[1]] %>%
  filter(str_detect(channel.name, "Sky Sport")) %>% 
  select(starttime, primeVision)


data[[1]] %>%
  filter(str_detect(channel.name, "Sky Sport")) %>% 
  # filter(content.subgenre.name == "Basket") %>% 
  filter(programHighlights == "L" & content.episodeNumber > 0) %>% 
  select(starttime, eventTitle, content.subgenre.name) %>% 
  distinct() %>% 
  mutate(eventTitle = substr(eventTitle, 1, 50))

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
  
  dat %>%
    filter(str_detect(channel.name, "Sky Sport")) %>% 
    filter(programHighlights == "L" & content.episodeNumber > 0 & !str_detect(content.contentTitle, "News")) %>% # I think this gets live and excludes talk shows/news
    distinct() 
}
# sky_dat <- parse_sky_json(pull_sky_json())
# sky_dat %>% 
#   select(starttime, eventTitle, content.subgenre.name) %>% 
#   distinct() %>% 
#   mutate(eventTitle = substr(eventTitle, 1, 50))

prepare_sky_table <- function(
    parsed_games
    # TODO: hours to show, select sport / team / etc
){
  parsed_games %>% 
    mutate(starttime = ymd_hms(starttime)) %>% 
    select(starttime) %>% 
    mutate(
      data = date(starttime)
      , ora = glue("{hour(starttime)}:{sprintf('%02d', minute(starttime))}")
      , descrizione = glue("{ora} {eventTitle}")
      , gruppo = "tv_sports"
      , testo_immagine = descrizione
      , immagine = "tv_sports//glyphs//sport.png"
      , colore = "FFFFFF"
    ) 
}

parsed_games <- parse_sky_json(pull_sky_json())
