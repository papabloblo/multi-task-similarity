library(tidyverse)

calendar <- read_csv2("empirical-work/bikes/data/raw/calendar_madrid.csv")

calendar <- calendar %>% 
  rename(date = Dia,
         weekday = Dia_semana,
         holiday = `laborable / festivo / domingo festivo`
         # national = `Tipo de Festivo`,
         # name = Festividad
         ) %>% 
  select(date, holiday) %>% 
  mutate(
    date = parse_date(date, '%d/%m/%Y'),
    holiday = ifelse(holiday %in% c('festivo', 'Festivo'), 1, 0)
    )


# Save final dataframe
saveRDS(calendar, file = "empirical-work/bikes/data/clean_data/calendar.RDS")
