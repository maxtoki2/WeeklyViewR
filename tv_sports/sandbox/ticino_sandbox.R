library(httr)
library(jsonlite)
library(dplyr)

# All weekdays as in API
weekdays <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")

# Function to fetch hockey game for a given weekday
get_hockey_game <- function(day) {
  url <- paste0("https://mixor.teleticino.ch/api/v4/schedules/", day)
  
  res <- try(GET(url), silent = TRUE)
  if (inherits(res, "try-error") || status_code(res) != 200) {
    return(NULL)
  }
  
  txt <- content(res, as = "text", encoding = "UTF-8")
  j <- fromJSON(txt, flatten = TRUE)
  
  schedules <- j$data$schedules
  
  # Extract only the actual hockey game
  game <- schedules %>%
    filter(grepl("^IL MATCH", title, ignore.case = TRUE)) %>%
    mutate(day = day)
  
  if (nrow(game) == 0) return(NULL)
  game
}

# Apply across all weekdays
hockey_games <- do.call(rbind, lapply(weekdays, get_hockey_game))

print(hockey_games)
