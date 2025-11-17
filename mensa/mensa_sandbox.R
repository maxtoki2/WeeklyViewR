get_menu <- function(
  pdf_url = "https://www.comune.sassomarconi.bologna.it/upload/sassomarconi_ecm8v2/gestionedocumentale/ScuoledellInfanzia_784_16225.pdf"
    , table_width = 2700
){
  # anno scolastico
  oggi <- today()
  anno1 <- year(oggi) - ifelse(month(oggi) > 8, 0, 1)
  anno_scolastico_vec <- c(anno1, anno1 + 1)
  
  menu <- image_read_pdf(pdf_url, 1) 
  
  menu2 <- menu %>% 
    # image_crop(crop_coordinates)  %>% 
    image_threshold(type = "white", threshold = "70%") %>% 
    image_morphology(method = "Dilate", kernel = "Rectangle:1x2") 
  
  ocr_info <- menu2 %>% 
    image_ocr_data("ita") %>% 
    separate(bbox, c("bb_xstart", "bb_ystart", "bb_xend", "bb_yend")) %>% 
    mutate(across(starts_with("bb"), as.numeric))
  
  table_top <- ocr_info %>% filter(word == "Giorni") %>% select(bb_yend) %>% unlist # min(days_col$bb_ystart)
  table_bottom <- ocr_info %>% filter(word == "completamento") %>% select(bb_ystart) %>% unlist #max(days_col$bb_yend)
  table_height <- plyr::round_any(table_bottom - table_top, 50, ceiling)
  
  dates_col <- ocr_info %>% 
    filter(str_detect(word, regex("\\b(0[1-9]|[12][0-9]|3[01])/(0[1-9]|1[0-2])\\b"))) %>% 
    filter(bb_ystart > table_top)

  table_left <- plyr::round_any(max(dates_col$bb_xend), 50)
  
  crop_coordinates <- glue("{table_width}x{table_height}+{table_left}+{table_top}")  # "2400x1550+800+535"  
  
  bind_cols(
    dates_col %>% filter(row_number() %% 2 == 1) %>% rename(date_from = word)
    , dates_col %>% filter(row_number() %% 2 == 0) %>% select(word) %>% rename(date_to = word)
  ) %>% 
    mutate(space = coalesce(bb_ystart - lag(bb_yend), 120)) %>% 
    mutate(new_group = space > 100) %>% 
    mutate(gruppo_sett = cumsum(new_group)) %>% 
    select(date_from, date_to, gruppo_sett) -> week_boxes
  
  lapply(1:nrow(week_boxes), function(i){
    date_from <- dmy(paste0(week_boxes$date_from[i], "/2020"))
    date_to <-   dmy(paste0(week_boxes$date_to[i], "/2020"))
    gruppo <- week_boxes$gruppo_sett[i]
    year(date_from) <- anno_scolastico_vec[ifelse(month(date_from) >= 9, 1, 2)]
    year(date_to) <- anno_scolastico_vec[ifelse(month(date_to) >= 9, 1, 2)]
    data.frame(
      data = seq.Date(date_from, date_to, "1 day")
      , gruppo_sett = gruppo
    )
  }) %>% 
    bind_rows() %>% 
    mutate(giorno_sett = wday(data, week_start = 1)) -> giorni_gruppo

  # getting menu entries
  menu %>% 
    image_crop(crop_coordinates)  %>%
    # image_threshold(type = "white", threshold = "70%") %>% 
    image_morphology(method = "Dilate", kernel = "Rectangle:1x2") %>% 
    image_ocr_data("ita") %>%
    separate(bbox, c("bb_xstart", "bb_ystart", "bb_xend", "bb_yend")) %>% 
    mutate(across(starts_with("bb"), as.numeric)) %>% 
    mutate(space = bb_xstart - lag(bb_xend)) -> menu_text_info
  
  menu_text_split <- menu_text_info %>% 
    mutate(space = coalesce(space, -2000)) %>% 
    mutate(new_row = space < 0) %>% 
    mutate(new_col = space > 30 | new_row) %>%
    mutate(row_group = cumsum(new_row)) %>% 
    # mutate(entry_num = cumsum(new_row | new_col)) %>% 
    group_by(row_group) %>% 
    mutate(col_group = cumsum(new_col)) %>% 
    ungroup()
  
  menu_entries <- menu_text_split %>% 
    group_by(row_group, col_group) %>% 
    summarise(descrizione = paste(word, collapse = " ")) %>% 
    ungroup() %>% 
    mutate(gruppo_sett = (row_group - 1) %/% 5 + 1) %>% 
    mutate(giorno_sett = row_group %% 5) %>% 
    mutate(giorno_sett = ifelse(giorno_sett == 0, 5, giorno_sett))
  
  menu_entries %>% 
    inner_join(giorni_gruppo, by = c("gruppo_sett", "giorno_sett"), relationship = "many-to-many") %>% 
    arrange(data, col_group) %>% 
    select(data, descrizione)
  
}

