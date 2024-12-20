library(tidyverse)


files <- list.files("empirical-work/bikes/data/raw/bicimad", full.names = TRUE)

import_file <- function(file_path){
  df <- read_delim(
    file = file_path,
    delim = ";"
  )
  
  df <- select(df, 
               fecha,
               fleet,
               trip_minutes, 
               unlock_date,
               lock_date, 
               station_unlock,
               dock_unlock,
               station_lock, 
               dock_lock)
  df <- na.omit(df)
  return(df)
}

all_files <- map(files, import_file) %>% 
  list_rbind()

saveRDS(all_files, file = "empirical-work/bikes/data/trips-bicimad.RDS")


# DATOS METEOROLÓGICOS ----------------------------------------------------

files <- list.files("empirical-work/bikes/data/raw/meteo", full.names = TRUE)

col_types <- cols(
  PROVINCIA = col_double(),
  MUNICIPIO = col_character(),
  ESTACION = col_double(),
  MAGNITUD = col_double(),
  PUNTO_MUESTREO = col_character(),
  ANO = col_double(),
  MES = col_double(),
  DIA = col_double(),
  H01 = col_double(),
  V01 = col_character(),
  H02 = col_double(),
  V02 = col_character(),
  H03 = col_double(),
  V03 = col_character(),
  H04 = col_double(),
  V04 = col_character(),
  H05 = col_double(),
  V05 = col_character(),
  H06 = col_double(),
  V06 = col_character(),
  H07 = col_double(),
  V07 = col_character(),
  H08 = col_double(),
  V08 = col_character(),
  H09 = col_double(),
  V09 = col_character(),
  H10 = col_double(),
  V10 = col_character(),
  H11 = col_double(),
  V11 = col_character(),
  H12 = col_double(),
  V12 = col_character(),
  H13 = col_double(),
  V13 = col_character(),
  H14 = col_double(),
  V14 = col_character(),
  H15 = col_double(),
  V15 = col_character(),
  H16 = col_double(),
  V16 = col_character(),
  H17 = col_double(),
  V17 = col_character(),
  H18 = col_double(),
  V18 = col_character(),
  H19 = col_double(),
  V19 = col_character(),
  H20 = col_double(),
  V20 = col_character(),
  H21 = col_double(),
  V21 = col_character(),
  H22 = col_double(),
  V22 = col_character(),
  H23 = col_double(),
  V23 = col_character(),
  H24 = col_double(),
  V24 = col_character()
)

import_file <- function(file_path){
  df <- read_delim(
    file = file_path,
    delim = ";",
    col_types = col_types
  ) %>% 
    select(-MUNICIPIO, -PROVINCIA, -starts_with("V"),
           -PUNTO_MUESTREO)
  return(df)
}


all_files <- map(files, import_file) %>% 
  list_rbind()

saveRDS(all_files, file = "empirical-work/bikes/data/meteo.RDS")
