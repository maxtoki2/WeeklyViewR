# nhl schedule scraping
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

# logos here: https://api-web.nhle.com/v1/schedule-calendar/2025-10-07

get_nhl_schedule <- function(date) {
  # date should be "YYYY-MM-DD"
  url <- paste0("https://api-web.nhle.com/v1/schedule/", date)
  
  res <- httr::GET(url)
  httr::stop_for_status(res)
  
  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  j <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
  
  # Inspect structure:
  # str(j)
  
  # Assuming j$data or j$dates (depending on API version)
  # The Zmalski reference says schedule is under top-level “dates”
  lapply(1:length(j$gameWeek$date), function(i){
    games_df <- j$gameWeek[i,"games"][[1]] 
    if(nrow(games_df) > 0){
      games_df %>% 
        unnest(cols = c(venue, tvBroadcasts, awayTeam, homeTeam, periodDescriptor), names_sep = "_") %>% 
        select(startTimeUTC, awayTeam_abbrev, homeTeam_abbrev)
    } else {
      games_df
    }
      
  }) %>% 
    bind_rows() %>%
    distinct() %>% 
    mutate(
      time_utc = ymd_hms(startTimeUTC, tz = "UTC"),
      time_cet = with_tz(time_utc, "Europe/Zurich")
    )
  
  
}

# Example usage:
df_oct8 <- get_nhl_schedule(today())
print(df_oct8)

df_oct8 %>% filter(hour(time_cet) > 6)

get_nhl_schedule(today() + days(2)) %>% arrange(startTimeUTC) %>% tail()
