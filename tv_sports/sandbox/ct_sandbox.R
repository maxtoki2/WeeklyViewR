library(rvest)
library(dplyr)
library(purrr)
library(stringr)

url <- "https://www.ceskatelevize.cz/tv-program/06.09.2025/"
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
  time = entries %>% html_node(".time, strong") %>% html_text(trim = TRUE),
  title = entries %>% html_node("a, strong, .porad") %>% html_text(trim = TRUE),
  full = entries %>% html_text(trim = TRUE)
)

sports_df
# entries[1] %>% html_nodes("*")