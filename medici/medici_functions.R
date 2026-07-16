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