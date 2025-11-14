anno_scolastico <- "2025/2026"
anno_scolastico_vec <- str_split(anno_scolastico, "/") %>% unlist %>% as.integer

menu <- image_read_pdf(pdf_url, 1) 

menu2 <- menu %>% 
  # image_crop(crop_coordinates)  %>% 
  image_threshold(type = "white", threshold = "70%") %>% 
  image_morphology(method = "Dilate", kernel = "Rectangle:1x2") 

ocr_info <- menu2 %>% 
  image_ocr_data("ita") %>% 
  separate(bbox, c("bb_xstart", "bb_ystart", "bb_xend", "bb_yend")) %>% 
  mutate(across(starts_with("bb"), as.numeric))

dates_col <- ocr_info %>% 
  filter(str_detect(word, regex("\\b(0[1-9]|[12][0-9]|3[01])/(0[1-9]|1[0-2])\\b"))) %>% 
  filter(bb_ystart > table_top)



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

menu_table <- menu_entries %>% 
  inner_join(giorni_gruppo, by = c("gruppo_sett", "giorno_sett"), relationship = "many-to-many") %>% 
  arrange(data, col_group) %>% 
  select(data, descrizione)


menu_table %>% filter(data == "2025-11-13")  
