library(tidyverse)

meteo <- readRDS("empirical-work/bikes/data/meteo.RDS")

# Translate variable names to English
meteo <- rename(meteo, 
                station = ESTACION, 
                magnitude = MAGNITUD,
                year = ANO,
                month = MES,
                day = DIA)

# Transpose measures from columns to rows
meteo <- meteo %>% 
  pivot_longer(
    cols = H01:H24,
    names_to = "hour",
    values_to = "value")

# Remove H from values and convert to double
meteo$hour <- as.numeric(str_remove(meteo$hour, "H"))

# Build datetime variable and remove year, month and day variables
meteo <- meteo %>% 
  mutate(
    date = make_datetime(year, month, day, hour)
    ) %>% 
  select(
    date, 
    magnitude, 
    station,
    value
    )

# Summarise magnitudes by datetime (ignoring station)
meteo <- meteo %>% 
  group_by(date, magnitude) %>% 
  summarise(value = mean(value)) %>% 
  ungroup()

# Convert magnitude codes to descriptive names
meteo <- meteo %>% 
  mutate(
    magnitude = case_match(
      magnitude,
      80 ~ "radiation_uv",
      81 ~ "wind_speed",
      82 ~ "wind_dir",
      83 ~ "temperature",
      86 ~ "humidity",
      87 ~ "pressure",
      88 ~ "radiation_sun",
      89 ~ "precipitation"
    )
  )

# Transpose the dataframe to have a column for each magnitude
meteo <- meteo %>% 
  pivot_wider(
    names_from = magnitude,
    values_from = value
  )


# Correct errors in magnitudes
meteo <- meteo %>% 
  mutate(
    humidity = if_else(humidity > 100, 100, humidity),
    pressure = if_else(pressure < 940, 940, pressure),
    pressure = if_else(pressure > 980, 980, pressure),
    wind_speed = if_else(wind_speed > 5, 5, wind_speed),
    precipitation = if_else(precipitation > 5, 5, precipitation)    
  )


# Save final dataframe
saveRDS(meteo, file = "empirical-work/bikes/data/clean_data/meteo.RDS")
