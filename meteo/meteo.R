#https://api.open-meteo.com/v1/forecast?latitude=44.3987&longitude=11.2481&daily=temperature_2m_max,temperature_2m_min,precipitation_probability_max&timezone=Europe%2FBerlin&past_days=1
# https://api.open-meteo.com/v1/forecast?latitude=44.3987&longitude=11.2481&hourly=temperature_2m,precipitation_probability,precipitation,cloud_cover&timezone=Europe%2FBerlin&past_days=14&forecast_days=14

get_meteo <- function(
    lat = 44.3987
    , lon = 11.2481
    , url = glue("https://api.open-meteo.com/v1/forecast?latitude={lat}&longitude={lon}&daily=temperature_2m_max&timezone=Europe/Rome")
){
  response <- GET(url)
  weather_data <- content(response, as = "text")
  fromJSON(weather_data)$hourly
}