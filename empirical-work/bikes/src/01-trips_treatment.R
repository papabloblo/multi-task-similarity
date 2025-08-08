library(tidyverse)

trips <- readRDS("empirical-work/bikes/data/trips-bicimad.RDS")

# Round datetime down to the nearest hour
# For example: '2021-06-01 00:00:53' -> '2021-06-01 00:00:00'
trips <- trips %>% 
  mutate(
    date = floor_date(unlock_date, unit = 'hour')
    )


# Count trips by hour and station unlock
trips <- trips %>% 
  group_by(station_unlock, date) %>% 
  summarise(n = n()) %>% 
  ungroup()


# If an hour is missing from the dataset,
# it means there were no trips during that hour, so its value should be set to 0.

stations_period <- trips %>% 
  group_by(station_unlock) %>% 
  summarise(
    min_date = min(date),
    max_date = max(date)
    )


expand_dates <- function(station_unlock, min_date, max_date){
  return(tibble(station_unlock = station_unlock, 
                date = seq(min_date, max_date, by = '1 hour')))
}

df <- pmap(stations_period, expand_dates)

all_hours <- bind_rows(df)
all_hours$n <- 0

trips <- trips %>% 
  full_join(
    all_hours, 
    by = join_by(station_unlock, date)
    ) %>% 
  mutate(n = ifelse(is.na(n.x), n.y, n.x)) %>% 
  select(station_unlock, date, n)

# Include past number of trips to use in the prediction.

# Previous hours to use
window <- 5
for (w in 1:window) {
  trips_aux <- trips %>% 
    select(station_unlock, date, n) %>% 
    mutate(date = date + hours(w)) 
  
  n_name <- paste0('n_lag', w)
  
  names(trips_aux)[names(trips_aux) == 'n'] <- n_name
  
  trips <- trips %>% 
    left_join(trips_aux, join_by(station_unlock, date))
  
  # If n_lagw is NA, it means that there were no trips.
  trips[[n_name]][is.na(trips[[n_name]])] <- 0
}



# trips <- trips %>% 
#   mutate(
#     hour = hour(unlock_date))
# 
# trips <- trips %>% 
#   mutate(
#     year = year(date),
#     month = month(date),
#     day = day(date),
#     wday = wday(date)
#     )

saveRDS(trips, file = "empirical-work/bikes/data/clean_data/trips.RDS")
