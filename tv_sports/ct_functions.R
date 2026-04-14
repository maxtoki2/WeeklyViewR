# CT sports functions
pull_ct_data <- function(date = today(), baseurl = "https://www.ceskatelevize.cz/tv-program"){
  url <- glue("{baseurl}/{str_pad(day(date), 2, 'left', 0)}.{str_pad(month(date), 2, 'left', 0)}.{year(date)}")
  read_html(url)
}

parse_ct_data <- function(page){
  # Find all headings, locate the "ČT sport" one
  # TODO: make it work for CT sport plus
  headers <- page %>% html_nodes("h3")
  idx <- which(str_detect(html_text(headers), "ČT sport"))
  
  if (length(idx) == 0) stop("ČT sport heading not found") # TODO: don't throw error which would screw everything else
  
  # Select subsequent sibling until next h3
  siblings <- headers[idx] %>% html_node(xpath = "following-sibling::*") # TODO: not properly parsing CT sport plus
  entries <- siblings %>% html_nodes("li")
  
  # Build tibble with time and title
  sports_df <- tibble(
    time = entries %>% html_node(".progTime") %>% html_text(trim = TRUE),
    # title = entries %>% html_node("a, strong, .porad") %>% html_text(trim = TRUE),
    sport = entries %>% html_node(".blockTitle") %>% html_text(trim = TRUE),
    descr = entries %>% html_node(".progTitle") %>% html_text(trim = TRUE),
    descr2 = entries %>% html_node("h5") %>% html_text(trim = TRUE),
    #full = entries %>% html_text(trim = TRUE)
    live = entries %>%  html_element('span[title="Živě"]') %>% html_text2()
  ) %>% 
    filter(!is.na(sport) & live == "L")
  
  sports_df
}


prepare_ct_table <- function(dates_list = periodo){
  lapply(dates_list, function(dt){
    parse_ct_data(pull_ct_data(dt)) %>% bind_cols(date = dt)
  }) %>% 
    bind_rows() %>% 
    filter(!str_detect(descr2, str_c(lista_esclusione, collapse = "|"))) %>% # TODO: scope
    inner_join(ct_parameters$translations, by = c("sport" = "czech")) %>% 
    rename(data = date, ora = time) %>% 
    mutate(
      descrizione = glue("{ora} ČT {descr2}") # TODO: translate? Add descr?
      , gruppo = "tv_sports"
      , testo_immagine = descrizione
      , immagine = glue("tv_sports/glyphs/{tolower(italian)}.png")
      , colore = NA
    )
}
