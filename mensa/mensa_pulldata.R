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
  mensa_periodo <- get_menu(param_url_mensa)
  saveRDS(mensa_periodo, glue("{folder_mensa}/{file_mensa}"))
}
mensa <- mensa_periodo %>% 
  filter(data %in% periodo) %>% 
  mutate(gruppo = "mensa") #%>% 
  # assign_cell()
mensa