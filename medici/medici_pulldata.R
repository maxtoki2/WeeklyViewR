print("medici")

source("medici/medici_functions.R")

medici <- lapply(param_id_medici, orari_medici) %>%
  bind_rows() %>% 
  mutate(gruppo = "medici") %>% 
  mutate(day = tolower(day)) %>% 
  mutate(day = ifelse(day %in% levels(wday(1:7, label = T, abbr = F)), day, as.character(wday(5, T, F)))) %>% # TODO: generalizza typo fix giovedi
  mutate(giorno_nr = numero_giorno_sett(day)) %>% 
  inner_join(
    data.frame(data = periodo, giorno_nr = wday(periodo))
    , by = "giorno_nr"
    , relationship = "many-to-many"
  ) %>% 
  mutate(descrizione = paste(abbr, hours, coalesce(ifelse(principale, "Sasso", "Borgo"), ""), " ", sep = "\n"))
medici
