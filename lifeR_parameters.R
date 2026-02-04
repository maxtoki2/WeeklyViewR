# collection of all parameters

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
    # , "Altri", " ", c("padel", "judo", lista_esclusione), "sport.png"
    # , "Tennis", lista_esclusione, "tennis.png"
  )
  , valid_hours = 7:22
)
  
rsi_parameters <- list(
  exclude_list = c("Altri sport", "Calcio")
)
