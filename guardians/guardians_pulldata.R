############
# guardians
############
print("guardians")
source("mlb/mlb_functions.R")
baseurl_cle_logo <- "https://www.mlbstatic.com/team-logos"
orig_filext_cle <- "svg"
outp_filext_cle <- "png"
folder_cle <- "guardians/saved"
file_cle <- glue("cle{min(periodo)}_{max(periodo)}.RDS")
if(file_cle %in% list.files(folder_cle)){
  cle_periodo <- readRDS(glue("{folder_cle}/{file_cle}"))  
} else {
  cle_periodo <- get_mlb_schedule(inizio_settimana, param_giorni, c(114, 445, 402, 437, 481), base_url = "https://bdfed.stitch.mlbinfra.com/bdfed/transform-milb-schedule?stitch_env=prod&sortTemplate=5&sportId=11&&sportId=12&&sportId=13&&sportId=14&&sportId=16")
  saveRDS(cle_periodo, glue("{folder_cle}/{file_cle}"))  
}
if(exists("mlb_periodo")){
  cle_periodo <- mlb_periodo %>% bind_rows(cle_periodo)
}
# TODO: formatta, combina immagini

if(!is.null(cle_periodo)){
  # info periodo scelto
  guardians <- cle_periodo %>% 
    filter(data %in% periodo) %>% 
    mutate(gruppo = "guardians")
  
  # scarica immagini
  lapply(unique(guardians$sel_team_id), function(x){
    get_images(x, orig_filext_cle, base_url = baseurl_cle_logo, save_folder = folder_cle, save_to_extension = outp_filext_cle)
  }) %>% 
    invisible()
  
  guardians <- guardians %>% 
    mutate(immagine = as.character(glue("{folder_cle}/{sel_team_id}.{outp_filext_cle}"))) #%>% 
  # assign_cell()
} else {
  guardians <- NULL
}
guardians
