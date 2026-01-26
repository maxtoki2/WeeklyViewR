# collection of all parameters

sky_parameters <- list(
  keywords = tibble::tribble(
    ~Sport, ~kw_include, ~kw_exclude, ~glyph
    , "Basket", c("nba", "virtus"), c("prepartita", "postpartita"), "basket.png"
    , "Motori", NA, c("test"), "motori.png"
    , "Calcio", c("bologna"), c("prepartita", "postpartita"), "calcio.png"
  )
)
  
  