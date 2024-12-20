library(tidyverse)

meteo <- readRDS("empirical-work/bikes/data/meteo.RDS")


meteo <- meteo %>% 
  pivot_longer(
    cols = H01:H24,
    names_to = "HORA",
    values_to = "VALOR")


meteo$HORA <- as.numeric(str_remove(meteo$HORA, "H"))

meteo <- meteo %>% 
  group_by(MAGNITUD, ANO, MES, DIA, HORA) %>% 
  summarise(VALOR = mean(VALOR)) %>% 
  ungroup()


meteo <- meteo %>% 
  mutate(
    MAGNITUD = case_match(
      MAGNITUD,
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

meteo <- meteo %>% 
  pivot_wider(
    names_from = MAGNITUD,
    values_from = VALOR
  )


meteo <- rename(meteo, 
                "year" = "ANO",
                "hour" = "HORA",
                "day" = "DIA",
                "month" = "MES",
)


meteo$date <- make_date(meteo$year, meteo$month, meteo$day)

meteo$date <- if_else(meteo$hour == 24, meteo$date + 1, meteo$date)
meteo$hour <- if_else(meteo$hour == 24, 0, meteo$hour)

meteo <- meteo %>% 
  mutate(
  year = year(date),
  month = month(date),
  day = day(date),
  wday = wday(date)
) %>% 
  select(-date)

saveRDS(meteo, file = "empirical-work/bikes/data/clean_data/meteo.RDS")
