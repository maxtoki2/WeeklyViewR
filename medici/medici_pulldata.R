print("medici")

source("medici/medici_functions.R")

medici <- lapply(param_id_medici, orari_medici) %>%
  bind_rows() %>% 
  mutate(gruppo = "medici") %>% 
  mutate(day = ifelse(day == "Gìovedì", "Giovedì", day)) %>% 
  mutate(giorno_nr = numero_giorno_sett(day)) %>% 
  inner_join(
    data.frame(data = periodo, giorno_nr = wday(periodo))
    , by = "giorno_nr"
    , relationship = "many-to-many"
  ) %>% 
  mutate(descrizione = paste(abbr, hours, ifelse(principale, "SM", "BN"), "---", sep = "\n"))
