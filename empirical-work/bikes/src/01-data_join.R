library(tidyverse)


meteo <- readRDS("empirical-work/bikes/data/clean_data/meteo.RDS")
trips <- readRDS("empirical-work/bikes/data/clean_data/trips.RDS")


trips <- left_join(trips, meteo)

trips <- na.omit(trips)


trips %>% 
  select(
    station_unlock,
    month, 
    wday,
    hour, 
    wind_speed,
    wind_dir,
    temperature,
    humidity,
    pressure,
    radiation_sun,
    precipitation,
    n
    )
