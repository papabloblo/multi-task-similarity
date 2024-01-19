#' IMPORTING PARKINSON DATASET
#' 
#' Source: https://archive.ics.uci.edu/dataset/189/parkinsons+telemonitoring


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)


# .CSV IMPORTATION --------------------------------------------------------

parkinson <- read_csv("empirical-work/real-data/data/parkinsons+telemonitoring/parkinsons_updrs.data")


parkinson <- parkinson %>% 
  rename("id_task" = `subject#`,
         jitter_perc = `Jitter(%)`,
         jitter_abs = `Jitter(Abs)`,
         jitter_rap = `Jitter:RAP`,
         jitter_ppq5 = `Jitter:PPQ5`,
         jitter_ddp = `Jitter:DDP`,
         shimmer_db = `Shimmer(dB)`,
         shimmer_apq3 = `Shimmer:APQ3`,
         shimmer_apq5 = `Shimmer:APQ5`,
         shimmer_apq11 = `Shimmer:APQ11`,
         shimmer_dda = `Shimmer:DDA`) %>% 
  select(-motor_UPDRS,
         -age,
         -sex)


parkinson$id_task <- as.character(parkinson$id_task)

write_rds(parkinson, "empirical-work/real-data/data/tasks_data.RDS")






