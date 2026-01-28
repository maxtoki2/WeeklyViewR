# RSI
library(httr2)
library(dplyr)
library(purrr)
library(tidyr)

# Define URL
url <- "https://1uf2cfvii9.execute-api.eu-central-1.amazonaws.com/events-by-date"

# Query API
resp <- request(url) |>
  req_url_query(
    fromDate = "2026-01-27Z",
    toDate   = "2026-01-30Z"
  ) |>
  req_perform()

# Parse JSON
events <- resp |> resp_body_json(simplifyVector = TRUE)

# Explore structure
str(events, max.level = 2)

# Convert to tibble if nested
events_df <- as_tibble(events)

# Keep only sports entries (assuming column/category field exists)
sports_df <- events_df %>%
  filter(streamtype == "sport") %>% 
  select(sport, dateTimeInfo, displayTitle) %>% 
  unnest(cols = c(sport, dateTimeInfo)) %>% 
  select(name, fullDateTime, displayTitle)

sports_df
