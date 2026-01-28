# collection of all parameters

sky_parameters <- list(
  keywords = tibble::tribble(
    ~Sport, ~kw_include, ~kw_exclude, ~glyph
    , "Basket", c("nba", "virtus"), c("prepartita", "postpartita", "approfondimento", "news"), "basket.png"
    , "Motori", "xxx", c("test"), "motori.png"
    , "Calcio", c("bologna"), c("prepartita", "postpartita", "approfondimento", "news"), "calcio.png"
    , "Rugby", c("Six Nations"), c("prepartita", "postpartita", "approfondimento", "news"), "rugby.png"
  )
  , valid_hours = 7:22
)
  
rsi_parameters <- list(
  exclude_list = c("Altri sport", "Calcio")
)
