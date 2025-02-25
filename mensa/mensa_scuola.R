get_menu <- function(
  date_from = "21-10-2024"
  , date_to = "07-03-2025"
  , pdf_url = "https://www.comune.sassomarconi.bologna.it/upload/sassomarconi_ecm8v2/gestionedocumentale/ScuoledellInfanzia_784_16225.pdf"
  , table_width = 2400
){
  menu <- image_read_pdf(pdf_url)
  
  date_from <- dmy(date_from)
  date_to <- dmy(date_to)
  # start_year <- year(date_from)
  dates_expanded <- data.frame(
    data = seq(date_from, date_to, by = "1 day")
  ) %>% 
    mutate(giorno_sett = wday(data, week_start = 1)) %>% 
    filter(giorno_sett < 6) %>% 
    mutate(anno = year(data)) %>% 
    mutate(new_week = (giorno_sett == 1)) %>% 
    mutate(settimana = cumsum(new_week)) %>% 
    mutate(gruppo_sett = settimana %% 5) %>% 
    mutate(gruppo_sett = ifelse(gruppo_sett == 0, 5, gruppo_sett))

  # general OCR
  ocr_info <- menu %>% 
    image_ocr_data("ita") %>% 
    separate(bbox, c("bb_xstart", "bb_ystart", "bb_xend", "bb_yend")) %>% 
    mutate(across(starts_with("bb"), as.numeric))
  
  # weekdays and dates columns to infer table position and dimensions
  days_col <- ocr_info %>% filter(word %in% c("LUN","MAR", "MER", "GIO", "VEN"))
  table_top <- min(days_col$bb_ystart)
  table_bottom <- max(days_col$bb_yend)
  table_height <- plyr::round_any(table_bottom - table_top, 50, ceiling)

  dates_col <- ocr_info %>% 
    filter(str_detect(word, regex("\\b(0[1-9]|[12][0-9]|3[01])/(0[1-9]|1[0-2])\\b"))) %>% 
    filter(bb_ystart > table_top)
  
  table_left <- plyr::round_any(max(dates_col$bb_xend), 50)
  
  crop_coordinates <- glue("{table_width}x{table_height}+{table_left}+{table_top}")  # "2400x1550+800+535"  
  
  # getting menu entries
  menu %>% 
    image_crop(crop_coordinates)  %>%
    image_threshold(type = "white", threshold = "70%") %>% 
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
  
  # attach dates
  dates_expanded %>% 
    select(data, gruppo_sett, giorno_sett) %>%  
    inner_join(menu_entries, by = c("gruppo_sett", "giorno_sett"), relationship = "many-to-many") %>%
    arrange(data, col_group) %>% 
    select(data, descrizione)
}

# get_menu()
