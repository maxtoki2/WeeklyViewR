extract_schedule <- function(part) {
  # Extract address (until first newline)
  addr <- str_match(part, "Studio medico:\\s*(.+)\\n")[,2]
  
  # Extract the schedule block after "Orari di ricevimento:"
  schedule_block <- str_match(part, "Orari di ricevimento:\\n([\\s\\S]+?)(\\n\\n|\\*$|$)")[,2]
  
  # Split schedule lines by newline and trim spaces
  schedule_lines <- str_trim(unlist(strsplit(schedule_block, "\n")))
  
  # Remove empty lines or lines starting with * (notes)
  schedule_lines <- schedule_lines[schedule_lines != "" & !startsWith(schedule_lines, "*")]
  
  # Parse day and hours
  schedule <- lapply(schedule_lines, function(line) {
    # Match "Day: hours" pattern
    m <- str_match(line, "^(\\p{L}+):\\s*(.+)$")
    if (is.na(m[1])) return(NULL)
    list(day = m[2], hours = m[3])
  })
  
  # Remove NULLs and convert to data.frame
  schedule <- do.call(rbind, lapply(schedule, as.data.frame))
  
  # Add address info
  if (!is.null(schedule)) {
    schedule$address <- addr
  }
  
  schedule
}

orari_medici <- function(
    id_medico = 12403 #11047 Boschi, 12403 Natali, 12418 Piccolantonio  
    , studio_princ = "BERTACCHI"
    , url = "https://www.progetto-sole.it/cercamedico/"
){
  # Prepare payload
  form_data <- list(
    medId = id_medico, #11047 Boschi, 12403 Natali, 12403 Piccolantonio, 16724 Degrassi
    # sbmType = "",
    page = "1",
    # nome = "",
    # cognome = "",
    tipomedico1 = "",  # or "1" (MMG), or "2" (PLS)
    comune = ""  # e.g., Bologna
  )
  
  # Send POST request
  res <- POST(url, body = form_data, encode = "form")
  
  # Parse the content
  html <- content(res, "text")
  # cat(substr(html, 1, 1000))  # Show the first 1000 chars for inspection
  
  text <- content(res) %>% html_nodes(".datimedico") %>% html_text2()
  
  name <- str_extract(text, "^[^-]+") |> str_trim()
  
  # Split by "Studio medico:" (keep the header by adding it back)
  parts <- unlist(strsplit(text, split = "Studio medico:", fixed = TRUE))[-1]
  parts <- parts[nzchar(parts)]  # remove empty elements
  
  # Add back "Studio medico:" prefix for each (optional)
  parts <- paste0("Studio medico:", parts)
  
  # Apply function to all parts
  schedules <- lapply(parts, extract_schedule)
  
  schedules %>% 
    bind_rows() %>% 
    mutate(medico = name, abbr = substr(name, 1, 3)) %>% 
    mutate(hours = str_remove(hours, "Su Appuntamento")) %>% 
    mutate(principale = str_detect(address, studio_princ))
}
