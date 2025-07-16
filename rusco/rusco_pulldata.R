#########
# rifiuti
#########
print("rifiuti")
source("rusco/rusco_functions.R")
baseurl_immagini_rusco <- "https://webapp-ambiente.gruppohera.it/assets/images/pictograms/"
filext_rusco <- "png"
folder_rusco <- "rusco/saved"
file_rusco <- glue("rusco{min(periodo)}_{max(periodo)}.RDS")
if(file_rusco %in% list.files(folder_rusco)){
  rusco_periodo <- readRDS(glue("{folder_rusco}/{file_rusco}"))  
} else {
  rusco_periodo <- get_calendar(inizio_settimana, param_giorni)
  saveRDS(rusco_periodo, glue("{folder_rusco}/{file_rusco}"))  
}
# TODO: formatta, combina immagini

# info periodo scelto
rusco <- rusco_periodo %>% 
  filter(data %in% periodo) %>% 
  mutate(gruppo = "rusco")

# scarica immagini
lapply(unique(rusco$nomeFile), function(x){
  get_images(x, filext_rusco, base_url = baseurl_immagini_rusco, save_folder = folder_rusco)
}) %>% 
  invisible()

rusco <- rusco %>% 
  #add_image_column(filename_column = "nomeFile", filext_rusco, save_folder = folder_rusco, base_url = baseurl_immagini_rusco) %>% 
  mutate(immagine = as.character(glue("{folder_rusco}/{nomeFile}.{filext_rusco}")))# %>% 
  # assign_cell()
