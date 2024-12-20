library(tidyverse)

trips <- readRDS("empirical-work/bikes/data/trips-bicimad.RDS")


trips <- trips %>% 
  mutate(
    hour = hour(unlock_date))


trips <- trips %>% 
  group_by(fecha, hour, station_unlock) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  rename("date" = "fecha")

trips <- trips %>% 
  mutate(
    year = year(date),
    month = month(date),
    day = day(date),
    wday = wday(date)
    )

saveRDS(trips, file = "empirical-work/bikes/data/clean_data/trips.RDS")
