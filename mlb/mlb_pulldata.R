############
# mlb
############
print("mlb")
source("mlb/mlb_functions.R")
baseurl_mlb_logo <- "https://www.mlbstatic.com/team-logos"
orig_filext_mlb <- "svg"
outp_filext_mlb <- "png"
folder_mlb <- "mlb/saved"
file_mlb <- glue("mlb{min(periodo)}_{max(periodo)}.RDS")
if(file_mlb %in% list.files(folder_mlb)){
  mlb_periodo <- readRDS(glue("{folder_mlb}/{file_mlb}"))  
} else {
  mlb_periodo <- get_mlb_schedule(inizio_settimana, param_giorni, 114)
  saveRDS(mlb_periodo, glue("{folder_mlb}/{file_mlb}"))  
}
# TODO: formatta, combina immagini

if(!is.null(mlb_periodo)){
  # info periodo scelto
  mlb <- mlb_periodo %>% 
    filter(data %in% periodo) %>% 
    mutate(gruppo = "mlb")
  
  # scarica immagini
  lapply(unique(mlb$opponent_id), function(x){
    get_images(x, orig_filext_mlb, base_url = baseurl_mlb_logo, save_folder = folder_mlb, save_to_extension = outp_filext_mlb)
  }) %>% 
    invisible()
  
  mlb <- mlb %>% 
    mutate(immagine = as.character(glue("{folder_mlb}/{opponent_id}.{outp_filext_mlb}"))) #%>% 
    # assign_cell()
} else {
  mlb <- NULL
}
mlb
