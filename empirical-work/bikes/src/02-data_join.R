library(tidyverse)


# IMPORT DATASETS ---------------------------------------------------------

meteo <- readRDS("empirical-work/bikes/data/clean_data/meteo.RDS")
trips <- readRDS("empirical-work/bikes/data/clean_data/trips.RDS")
calendar <- readRDS("empirical-work/bikes/data/clean_data/calendar.RDS")


# LAG WEATHER -------------------------------------------------------------

# To avoid using information unavailable at prediction time,
# weather variables have been lagged by one hour.
# Each value now corresponds to the magnitude from the previous hour.
trips$date <- trips$date + hours(1)


# JOIN DATASETS -----------------------------------------------------------

trips <- left_join(trips, meteo, by = join_by(date))

trips$date_aux <- date(trips$date)
trips <- left_join(trips, calendar, by = join_by(date_aux == date))
trips$date_aux <- NULL

# Extract month and hour variables.
# Generate week day variable (1=Monday, 2=Tuesday,...)

trips <- trips %>% 
  mutate(
    month = month(date),
    hour = hour(date),
    wday = wday(date, week_start = 1)
  )


# REMOVE STATIONS ---------------------------------------------------------

# Remove stations with less than 2000 hours of functioning

trips <- trips %>% 
  group_by(station_unlock) %>% 
  mutate(total_hours = n()) %>% 
  filter(total_hours >= 2000) %>% 
  select(-total_hours)


# TRAIN TEST SPLIT --------------------------------------------------------

# Build the test set using trips from the six months
# preceding each station’s latest available date

trips <- trips %>% 
  group_by(station_unlock) %>% 
  mutate(date_split = max(date(date)) - months(6)) %>% 
  ungroup()

test <- trips %>% 
  filter(date >= date_split) %>% 
  select(-date, -date_split)

train <- trips %>% 
  filter(date < date_split) %>% 
  select(-date, -date_split)


# # Se eliminan estaciones con menos de 2000 horas de funcionamiento
# station_delete <- trips %>% 
#   group_by(station_unlock) %>% 
#   summarise(n = n()) %>% 
#   filter(n < 2000)
# 
# trips <- trips[!(trips$station_unlock %in% station_delete$station_unlock), ]
# 
# min(table(trips$station_unlock))
# max(table(trips$station_unlock))
# 
# set.seed(1234)
# perc_train <- .7
# 
# tasks <- unique(trips$station_unlock)
# n_tasks <- length(tasks)
# 
# list_train <- vector("list", length = n_tasks)
# list_test <- vector("list", length = n_tasks)
# for (id_task in 1:n_tasks){
#   task <- tasks[id_task]
#   
#   df_task <- filter(trips, station_unlock == task)
#   n_df <- nrow(df_task)
#   id_train <- sample(1:n_df, size = n_df*perc_train)
#   
#   list_train[[id_task]] <- df_task[id_train, ]
#   list_test[[id_task]] <- df_task[-id_train, ]
#   
# }
# 
# train <- bind_rows(list_train)
# test <- bind_rows(list_test)
# 

  
write_csv(train, file = "empirical-work/bikes/data/bikes_train.csv")
write_csv(test, file = "empirical-work/bikes/data/bikes_test.csv")
saveRDS(trips, file = "empirical-work/bikes/data/dataset_model.RDS")
