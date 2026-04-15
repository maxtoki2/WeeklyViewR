library(jsonlite)
library(dplyr)
library(stringr)

url <- "https://epg.discovery.indazn.com/eu/v2/Epg?%24format=json&date=2026-04-15&country=IT&languageCode=it"

dazn_json <- fromJSON(url)

dazn_json$Tiles %>% filter(Label != "NHL.TV") %>% distinct(Sport)

dazn_json$Tiles %>% filter(Label != "NHL.TV") %>% filter(Sport$Title == "Hockey su ghiaccio")

dazn_json$Tiles %>% distinct(EntitlementIds)

dazn_json$Tiles %>% filter(str_detect(EntitlementIds, "base_dazn"))
