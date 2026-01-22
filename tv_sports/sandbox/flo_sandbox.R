library(rvest)
library(dplyr)
library(purrr)
library(stringr)

get_flohockey_events <- function(date) {
  # e.g. "2025-10-03"
  url <- paste0("https://www.flohockey.tv/events?date=", date)
  page <- read_html(url)
  
  # On the general events page, events are often grouped by league sections
  # We find league blocks (e.g. <section> or <div class="league-events">)
  league_nodes <- page %>% html_elements(".league-container, .events-group, .league-section")
  
  # If those don’t match, fallback to a generic event list
  if (length(league_nodes) == 0) {
    league_nodes <- list(page)
  }
  
  # For each league section, parse events
  events <- map_dfr(league_nodes, function(ln) {
    # Extract league name if available
    league_name <- ln %>%
      html_element(".league-title, .league-name, .events-header") %>%
      html_text(trim = TRUE)
    
    # Event entries
    ev_nodes <- ln %>% html_elements(".event, .schedule-event, .event-item")
    if (length(ev_nodes) == 0) return(tibble())
    
    map_dfr(ev_nodes, function(ev) {
      time <- ev %>% html_element(".event-time, .time") %>% html_text(trim = TRUE)
      match <- ev %>% html_element(".event-title, .title, .matchup") %>% html_text(trim = TRUE)
      status <- ev %>% html_element(".event-status, .status, .live-tag") %>% html_text(trim = TRUE)
      location <- ev %>% html_element(".event-location, .venue") %>% html_text(trim = TRUE)
      link <- ev %>% html_element("a") %>% html_attr("href")
      
      tibble(
        date = date,
        league = league_name,
        time = time,
        match = match,
        status = status,
        location = location,
        link = link
      )
    })
  })
  
  events
}

# Test for Oct 3
evs_oct3 <- get_flohockey_events("2025-10-03")
print(evs_oct3)
