##############
# mensa scuola
##############
print("mensa")
source("mensa/mensa_functions.R")
folder_mensa <- "mensa/saved"
file_mensa <- glue("mensa_{param_url_mensa_doc_id}.RDS")
if(file_mensa %in% list.files(folder_mensa)){
  mensa_periodo <- readRDS(glue("{folder_mensa}/{file_mensa}"))
} else {
  mensa_periodo <- lapply(1:2, function(x) get_menu(param_url_mensa, pagina = x)) 
  saveRDS(mensa_periodo, glue("{folder_mensa}/{file_mensa}"))
}

mensa_periodo <- bind_cols(mensa_periodo) %>% 
  setNames(c("data", "descrizione1", "data2", "descrizione2")) %>% 
  mutate(descrizione = ifelse(descrizione1 == descrizione2, descrizione1, paste(descrizione1, descrizione2, sep = " | "))) %>% 
  select(data, descrizione)

mensa <- mensa_periodo %>% 
  filter(data %in% periodo) %>% 
  mutate(gruppo = "mensa") #%>% 
  # assign_cell()
mensa