# collection of all parameters

secr_calendario_famiglia <- Sys.getenv("GCAL_FAMIGLIA")
secr_id_indirizzo <- Sys.getenv("ID_INDIRIZZO")
secr_id_civico <- Sys.getenv("ID_CIVICO")

# TODO: move to calendario?
param_calendari <- c(
  "famiglia" = glue("https://calendar.google.com/calendar/ical/qblk3l3l4vu78epu50r1k3jmj0%40group.calendar.google.com/private-{secr_calendario_famiglia}/basic.ics")
)

param_rusco_days_offset <- 1

param_url_mensa_base <- "https://www.comune.sassomarconi.bologna.it/myportal/C_G972/api/content/download"
param_url_mensa_doc_id <- "68f5e18203ce61009b6a9f32"

param_url_mensa <- glue("{param_url_mensa_base}?id={param_url_mensa_doc_id}")

param_id_medici <- c("boschi" = 11047, "zatti" = 13277,"alberghini" = 16841)


lista_esclusione <- c("prepartita", "postpartita", "approfondimento", "news", "presentazione", "test")

sky_parameters <- list(
  keywords = tibble::tribble(
    ~Sport, ~kw_include, ~kw_exclude, ~glyph
    , "Basket", c(" "), lista_esclusione, "basket.png"
    , "Motori", " ", lista_esclusione, "motori.png"
    , "Calcio", c("bologna"), lista_esclusione, "calcio.png"
    , "Rugby", c("Six Nations", "Sei Nazioni"), lista_esclusione, "rugby.png"
    , "Atletica", " ", lista_esclusione, "atletica.png"
    , "Volley", " ", lista_esclusione, "volley.png"
    , "Altri", " ", c("padel", "judo", lista_esclusione), "sport.png"
    # , "Tennis", lista_esclusione, "tennis.png"
  )
  , valid_hours = 7:22
)
  
rsi_parameters <- list(
  exclude_list = c("Altri sport", "Calcio")
)
