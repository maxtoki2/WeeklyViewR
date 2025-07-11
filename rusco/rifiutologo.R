get_calendar <- function(
    inizio = now()
    , giorni = 14
    , url = "https://www.ilrifiutologo.it/ajax/archivio_ilrifiutologo_ajax.php"
    , payload_url = "https://webapp-ambiente.gruppohera.it/rifiutologo/rifiutologoweb/getCalendarioPap.php"
    , id_comune = 46
    , id_indirizzo = secr_id_indirizzo
    , id_civico = secr_id_civico
){
  #start_date <- giorno - days(7)
  formatted_date <- format(inizio, "%Y-%m-%dT%H:%M:%S")
  # Create the payload
  payload <- list(
    url = payload_url
    , type = "GET"
    , parameters = glue('{"idComune":<<id_comune>>,"idIndirizzo":<<id_indirizzo>>,"idCivico":<<id_civico>>,"isBusiness":"0","date":"<<formatted_date>>","giorniDaMostrare":<<giorni>>}', .open = "<<", .close = ">>")
  )  
  
  # Send the POST request with extra headers
  page_status <- -1
  attempts_counter <- 0
  while(page_status != 200 & attempts_counter < 5){
    response <- POST(
      url,
      body = payload,
      encode = "form",  # Adjust as needed (form if form data, json if raw JSON is expected)
      add_headers(
        `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:89.0) Gecko/20100101 Firefox/89.0",  # Pretend to be a browser
        `Referer` = "https://www.ilrifiutologo.it/casa_rifiutologo/",  # Referrer from where the request is made
        `Origin` = "https://www.ilrifiutologo.it",  # Add the origin header
        `Accept` = "application/json",  # Request JSON response
        `Content-Type` = "application/x-www-form-urlencoded"  # Ensure correct content-type
      )
    )
    page_status <- response$status_code
    attempts_counter <- attempts_counter + 1
    if(page_status != 200) Sys.sleep(10)
  }
  
  response_content <- content(response, "text")
  if(page_status == 200 & response_content != ""){
    parsed_content <- fromJSON(response_content)$calendario
    formatted_content <- parsed_content %>% 
      #filter(week(data) == week(giorno)) %>% 
      unnest(conferimenti) %>% 
      mutate(data = as.Date(data)) %>% 
      select(data, macroprodotto) %>% 
      unnest(macroprodotto) %>% 
      unnest(pittogramma) %>% 
      mutate(testo_immagine = str_extract(descrizione, "^[A-Za-z]+"))  
  } else {
    formatted_content <- structure(list(data = structure(numeric(0), class = "Date"), 
                                        id = integer(0), descrizione = character(0), nomeFile = character(0), 
                                        colore = character(0), testo_immagine = character(0)), row.names = integer(0), class = c("tbl_df", 
                                                                                                                                 "tbl", "data.frame"))
  }
  
  formatted_content
}

# get_calendar()
