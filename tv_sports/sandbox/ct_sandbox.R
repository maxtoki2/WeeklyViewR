library(rvest)
library(dplyr)
# library(purrr)
library(stringr)

url <- "https://www.ceskatelevize.cz/tv-program/29.01.2026/"
page <- read_html(url)

# Find all headings, locate the "ČT sport" one
headers <- page %>% html_nodes("h3")
idx <- which(str_detect(html_text(headers), "ČT sport"))

if (length(idx) == 0) stop("ČT sport heading not found")

# Select subsequent sibling until next h3
siblings <- headers[idx] %>% html_node(xpath = "following-sibling::*")
entries <- siblings %>% html_nodes("li")

# Build tibble with time and title
sports_df <- tibble(
  time = entries %>% html_node(".progTime") %>% html_text(trim = TRUE),
  # title = entries %>% html_node("a, strong, .porad") %>% html_text(trim = TRUE),
  sport = entries %>% html_node(".blockTitle") %>% html_text(trim = TRUE),
  descr = entries %>% html_node(".progTitle") %>% html_text(trim = TRUE),
  descr2 = entries %>% html_node("h5") %>% html_text(trim = TRUE),
  #full = entries %>% html_text(trim = TRUE)
  live = entries %>%  html_element('span[title="Živě"]') %>% html_text2()
) %>% 
  filter(!is.na(sport) & live == "L")

sports_df %>% 
  mutate(
    sport = polyglotr::google_translate(sport, "it", "cs")
    # , descr2 = polyglotr::google_translate(descr2, "it", "cs")
  )
# entries[1] %>% html_nodes("*")

# print(schedule_live %>% mutate(titleIT = google_translate(title, "it", "sk")))