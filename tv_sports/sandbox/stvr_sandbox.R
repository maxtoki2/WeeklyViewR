library(rvest)
library(dplyr)
library(lubridate)
library(polyglotr)

get_sport_schedule <- function(date) {
  url <- paste0("https://www.stvr.sk/televizia/program?date=", date)
  
  # Read page
  page <- read_html(url)
  
  # Select the sport channel section
  sport_section <- page %>% html_element(".tv--sport .col-wrapper")
  if (is.na(sport_section)) return(NULL)
  
  programs <- sport_section %>% html_elements(".media__body")
  
  # Build tibble
  tibble(
    date = date,
    time = programs %>% html_element(".time--start") %>% html_text(trim = TRUE),
    title = programs %>% html_element(".title") %>% html_text(trim = TRUE),
    genre = programs %>% html_element(".genre") %>% html_text(trim = TRUE)
  ) %>% 
    filter(genre == "šport")
}

# Example: get sport schedule for 2025-09-19
schedule_live <- get_sport_schedule(today())
print(schedule_live %>% mutate(titleIT = google_translate(title, "it", "sk")))
