
get_calendars <- function(
    calendar_icss   
  ){
  calendar_list <- lapply(calendar_icss, function(x){
    ic_read(x)
  })
  names(calendar_list) <- names(calendar_icss)
  calendar_df <- bind_rows(calendar_list, .id = "gruppo") %>% 
    select(DTSTART, `DTSTART;VALUE=DATE`, SUMMARY, gruppo) %>% 
    mutate(datetime = coalesce(`DTSTART;VALUE=DATE`, DTSTART)) %>%  #floor_date(DTSTART, "day") %>% 
    arrange(datetime) %>% 
    mutate(data = as.Date(floor_date(datetime))) %>% 
    mutate(ora = format(datetime, "%H:%M")) %>% 
    mutate(ora_txt = ifelse(ora == "00:00", "-", ora)) %>% 
    mutate(flag_flights = identify_flight_entries(SUMMARY)) %>% 
    mutate(SUMMARY = ifelse(flag_flights, shorten_flight_entries(SUMMARY), SUMMARY)) %>% 
    mutate(descrizione = glue("({ora_txt}) {SUMMARY}")) %>% 
    mutate(descrizione = str_remove(descrizione, fixed("(-)")))
  calendar_df
}

identify_flight_entries <- function(
  calendar_text = "MASSIMILIANO MARCHI DL8421 Paris Charles De Gaulle -> Bologna G Mar\n coni Jan 26\\, 2025 12:10 PM (local departure time)"
  , fixed_start = "MASSIMILIANO MARCHI"
  , trim_after = "\\\\"
  ){
  str_detect(calendar_text, glue("^{fixed_start}")) & str_detect(calendar_text, trim_after)
}

shorten_flight_entries <- function(
    calendar_text = "MASSIMILIANO MARCHI DL8421 Paris Charles De Gaulle -> Bologna G Mar\n coni Jan 26\\, 2025 12:10 PM (local departure time)"
    , fixed_start = "MASSIMILIANO MARCHI"
    , trim_after = "\\\\"  
  ){
  calendar_text %>% 
    str_remove(glue("^{fixed_start} ")) %>% 
    str_replace(glue("{trim_after}.*"), "") %>% 
    str_replace("\n ", "") %>% 
    str_replace(" [A-Z][a-z]{2} [0-9]{1,2}$", "")
}
