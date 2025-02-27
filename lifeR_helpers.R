get_images <- function(
    pic_file # file name, no extension
    , file_extension # one of png or svg
    , save_folder
    , base_url # TODO: make optional (useful for add_image_column())
    # , bg_color = NA # desired background color (hex, no #)
    # , bg_original = NA # if not transparant, so it will be replace by above
    , save_to_extension = file_extension
    , save = TRUE
    , overwrite = FALSE
){
  #full_path <- glue("{base_url}/{pic_file}.{file_extension}")
  destination_file <- glue("{save_folder}/{pic_file}.{save_to_extension}")
  
  if((!file.exists(destination_file)) | overwrite){
    full_path <- glue("{base_url}/{pic_file}.{file_extension}")
    lookup_extension <- file_extension
  } else {
    full_path <- glue("{save_folder}/{pic_file}.{save_to_extension}")
    lookup_extension <- save_to_extension
  }
  
  if(lookup_extension == "png"){
    img_read_fun <- image_read
  }
  if(lookup_extension == "svg"){
    img_read_fun <- image_read_svg
  }
  
  img <- img_read_fun(full_path)

  if((!file.exists(destination_file)) | overwrite)
    image_write(img, destination_file, format = save_to_extension)
  
  # # change background
  # # TODO: move to a process function
  # if(!is.na(bg_color)){
  #   # first replace the original
  #   if(!is.na(bg_original)){
  #     img <- image_transparent(glue("#{bg_original}"))
  #   }
  #   img <- img %>% 
  #     image_background(glue("#{bg_color}"))
  # }  
  
  img
}

# get_images(
#   pic_file = "img7653"
#   , file_extension = "png"
#   , bg_color = "7C7C81"
#   , base_url = "https://webapp-ambiente.gruppohera.it/assets/images/pictograms"
#   , save_folder = "rusco/saved"
#   , overwrite = FALSE  
# )
# 
# get_images(
#   pic_file = "114"
#   , file_extension = "svg"
#   , base_url = "https://www.mlbstatic.com/team-logos"
#   , save_folder = "mlb/saved"
#   , overwrite = FALSE
# )

img_change_background <- function(
  img    
  , bg_color # desired background color (hex, no #)
  , bg_original = NA # if not transparant, so it will be replace by above
){
  if(!is.na(bg_original)){
    img <- img %>% image_transparent(glue("#{bg_original}"))
  }
  img <- img %>% 
    image_background(glue("#{bg_color}"))
  img
}

# for images stored as list in a data.frame cell
img_merge <- function(
    imgs #= c("img7653", "img6652")
){
  images <- unlist(imgs)
  list(image_append(do.call("c", images)))
}

# TODO: make genearl so it can be used elsewhere (e.g. MLB logos)
add_image_column <- function(
    formatted_content
    , filename_column 
    , file_extension
    , save_folder #= "rusco/saved"
    , base_url
){
  formatted_content <- formatted_content %>% as.data.frame()
  img_to_download <- unique(formatted_content[,filename_column])
  lapply(1:length(img_to_download), function(i){
    riga_download <- img_to_download[i]
    get_images(riga_download, file_extension, save_folder, base_url)
    image_read(glue("{save_folder}/{riga_download}.{file_extension}"))
  }) -> picto_list
  names(picto_list) <- img_to_download
  # formatted_content %>% 
  #   select(data, pittogramma) %>% 
  #   unnest() %>% 
  #   inner_join(., ., by = "data", suffix = c("1", "2")) %>% 
  #   filter(nomeFile1 != nomeFile2) %>% 
  #   select(data, starts_with("nome"))
  formatted_content$immagine <- rep(list(NULL), nrow(formatted_content))
  lapply(1:nrow(formatted_content), function(i){
    # formatted_content$immagine[[i]] <<- list(picto_list[[formatted_content$pittogramma$nomeFile[i]]])
    formatted_content$immagine[[i]] <<- list(picto_list[[formatted_content[i, filename_column]]])
  }) %>% invisible
  formatted_content
}

# TODO: can probably become part of assign_cell functional
# formatted_content %>%  group_by(data) %>% summarise(img = merge_pictograms(immagine)) %>% select(img) %>% slice(2) %>% unlist

# table formatting
table_prepare <- function(
  dat = tutto
  , w = 1
  , righe_tabella = 20
  , tbl_width = 1.5
  , tbl_height = .15
  , oggi =  giorno
  , sfondo_oggi = "ivory2"
){
  days_start <- inizio_settimana + (w - 1) * 7
  days_end <- days_start + 6
  dates <- seq(days_start, days_end, by = "1 day") 
  days <- dates %>% 
    wday(TRUE, FALSE, locale = "it_IT.UTF-8") %>% 
    as.character()
  # date_text <- format(dates, "%d %b", locale = "it_IT.UTF-8")
  
  planner_table <- as.data.frame(matrix("", nrow = righe_tabella, ncol = length(days)))
  colnames(planner_table) <- glue("{days} {day(dates)}")
  #planner_table[1, ] <- date_text
  
  tutto1 <- dat %>% filter(pagina == w) %>% as.data.frame
  for(i in 1:nrow(tutto1)){
    riga <- tutto1[i,]
    planner_table[riga$riga, riga$colonna] <- coalesce(riga$immagine, riga$descrizione) 
  }
  celle_con_immagine <- tutto1 %>% filter(!is.na(immagine)) %>% select(riga, colonna, colore) %>% as.data.frame()
  celle_con_solo_testo <- tutto1 %>% anti_join(celle_con_immagine, by = c("riga", "colonna")) %>% select(riga, colonna, descrizione, gruppo)
  celle_con_testo_immagine <- tutto1 %>% filter(!is.na(testo_immagine)) %>% select(riga, colonna, immagine, testo_immagine, colore) %>% as.data.frame()
  celle_con_solo_immagine <- celle_con_immagine %>% anti_join(celle_con_testo_immagine, by = c("riga", "colonna"))
  
  flextbl <- flextable(planner_table) %>% 
    width(width = tbl_width) %>% 
    height(height = tbl_height) #%>% 
    # hrule(rule = "exact")
  
  if(w == 1){
    flextbl <- flextbl %>% 
      bg(j = wday(oggi), bg = sfondo_oggi, part = "all")
  }
  
  if(nrow(celle_con_solo_testo) > 0){
    celle_da_unire <- celle_con_solo_testo %>% 
      group_by(colonna, gruppo) %>% 
      summarise(
        riga_inizio = min(riga)
        #, riga_fine = max(riga)
        , testo_unito = paste(descrizione, collapse = "|")
      ) %>% 
      cross_join(param_celle %>% select(riga1) %>% mutate(riga_fine = riga1 - 1)) %>% 
      filter(riga_fine > riga_inizio) %>% 
      group_by(colonna, riga_inizio) %>% 
      slice_min(riga_fine) %>% 
      ungroup() %>% 
      select(-gruppo, -riga1)
    for(i in 1:nrow(celle_da_unire)){
      colonna <- celle_da_unire$colonna[i]
      righe <- celle_da_unire$riga_inizio[i]:celle_da_unire$riga_fine[i]
      testo <- str_replace_all(celle_da_unire$testo_unito[i], fixed("|"), "\n")
      flextbl <- flextbl %>% 
        compose(i = righe[1], j = colonna, as_paragraph(as_chunk(testo))) %>% 
        merge_at(i = righe, j = colonna) %>% 
        valign(valign = "top") #TODO: shrink/resize long text
    }
  }
  
  if(nrow(celle_con_solo_immagine) > 0){
    for(i in 1:nrow(celle_con_solo_immagine)){
      flextbl <- flextbl %>% 
        colformat_image(i = celle_con_immagine[i, 1], j = celle_con_immagine[i, 2], width = .3, height = .3)
      if(!is.na(celle_con_immagine[i, 3])){
        flextbl <- flextbl %>% 
          bg(i = celle_con_immagine[i, 1], j = celle_con_immagine[i, 2], bg = paste0("#", celle_con_immagine[i, 3]))
      }
    }  
  }
  
  
  for(i in 1:nrow(celle_con_testo_immagine)){
    riga <- celle_con_testo_immagine[i,]
    cell_bg <- paste0("#", riga$colore)
    cell_txt_col <- TextContrastColor(cell_bg)
    flextbl <- flextbl %>% 
      compose(i = riga$riga, j = riga$colonna, value = as_paragraph(
        as_image(riga$immagine, width = .4, height = .4) # TODO: parametrize w/h
        , " "
        , as_chunk(riga$testo_immagine, props = fp_text_default(color = cell_txt_col, font.size = 8))) # TODO: parametrize size and pick text color based on background (DescTools::TextContrastColor)
        , part = "body"
      ) %>% 
      # valign(j = 2, valign = "center") %>% 
      bg(i = riga$riga, j = riga$colonna, bg = cell_bg)
  }
  
  for(i in 1:nrow(tutto1)){
    riga <- tutto1[i,]
    flextbl <- flextbl %>% 
      fontsize(i = riga$riga, j = riga$colonna, size = riga$font_size)
  }
  
  flextbl
}

# tables[[1]] %>% compose(i = 1:3, j = 1, value = as_paragraph(as_image("rusco/saved/img6649.png", width = .4, height = .4), as_image("rusco/saved/img7653.png", width = .4, height = .4), " ", as_chunk("test", props = fp_text_default(color = "white"))), part = "body") %>% bg(i = 1:3, j = 1, bg = "black")