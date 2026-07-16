# 28935 Alberghini
# 21086 Boschi
# 18624 Zatti

library(httr2)
library(dplyr)
library(glue)

resp <- request("https://api.progetto-sole.it/public/cercamedico/medico/28358") |>
  req_headers(
    Accept = "application/json",
    Origin = "https://cercamedico.progetto-sole.it",
    Referer = "https://cercamedico.progetto-sole.it/",
    `X-App-Id` = "CERCAMEDICOWEB",
    `X-App-Version` = "1.0.0"
  ) |>
  req_perform()

# json <- resp_body_json(resp)
# 
# str(json)
# 
# jsonlite::fromJSON(json)

obj <- resp_body_json(resp, simplifyVector = TRUE)
# str(obj, max.level = 2)
# names(obj)

obj$dettagliMedico$ambulatori$orari
obj$dettagliMedico$ambulatori$telefono
obj$dettagliMedico$ambulatori$cellulare
obj$dettagliMedico$ambulatori$comune
obj$dettagliMedico$ambulatori$localita

txt <- resp_body_string(resp)

# cat(txt)

jsonlite::prettify(txt)

# obj <-jsonlite::fromJSON(txt, simplifyVector = TRUE)

data.frame(
  medico = obj$dettagliMedico$cognome
  , telefono = obj$dettagliMedico$ambulatori$cellulare
) %>% 
  distinct()

obj$dettagliMedico$ambulatori$orari

orari <- lapply(1:nrow(obj$dettagliMedico$ambulatori), function(x){
  luogo <- coalesce(obj$dettagliMedico$ambulatori$localita[x], obj$dettagliMedico$ambulatori$comune[x])
  orari <- obj$dettagliMedico$ambulatori$orari[x]
  orari <- orari[[1]]
  orari$ambulatorio <- luogo
  orari
}) %>% bind_rows()

orari$medico <- obj$dettagliMedico$cognome

orari %>% 
  rename(day = giorno) %>% 
  bind_rows(
    data.frame(day = "Domenica", dalle = paste(unique(obj$dettagliMedico$ambulatori$cellulare), collapse = " - "), alle = "")
  ) %>% 
  mutate(
    abbr = toupper(medico)
    , hours = paste(dalle, alle, sep = "-")
    , principale = stringr::str_detect(ambulatorio, "Sasso")
  )

####
orari_medici <- function(id_medico = 28358, studio_princ = "Sasso", url = "https://api.progetto-sole.it/public/cercamedico/medico"){
  resp <- request(glue("{url}/{id_medico}")) |>
    req_headers(
      Accept = "application/json",
      Origin = "https://cercamedico.progetto-sole.it",
      Referer = "https://cercamedico.progetto-sole.it/",
      `X-App-Id` = "CERCAMEDICOWEB",
      `X-App-Version` = "1.0.0"
    ) |>
    req_perform()
  
  obj <- resp_body_json(resp, simplifyVector = TRUE)
 
  orari <- lapply(1:nrow(obj$dettagliMedico$ambulatori), function(x){
    luogo <- coalesce(obj$dettagliMedico$ambulatori$localita[x], obj$dettagliMedico$ambulatori$comune[x])
    orari <- obj$dettagliMedico$ambulatori$orari[x]
    orari <- orari[[1]]
    orari$ambulatorio <- luogo
    orari
  }) %>% bind_rows()
  
  orari$medico <- obj$dettagliMedico$cognome
  
  orari %>% 
    rename(day = giorno) %>% 
    bind_rows(
      data.frame(day = "Domenica", dalle = paste(unique(obj$dettagliMedico$ambulatori$cellulare), collapse = " - "), alle = "", medico = obj$dettagliMedico$cognome)
    ) %>% 
    mutate(
      abbr = toupper(medico)
      , hours = paste(dalle, alle, sep = "-")
      , principale = stringr::str_detect(ambulatorio, studio_princ)
    ) 
}
orari_medici(18624)

# 28935 Alberghini
# 21086 Boschi
# 18624 Zatti