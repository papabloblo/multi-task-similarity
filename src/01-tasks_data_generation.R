#' 
#' DATA SIMULATION FOR TASKS
#' 
#' 


# DEPENDENCIES ------------------------------------------------------------

library(MASS)
library(tidyverse)
library(EnvStats)
library(rjson)
library(sn)

source("src/00-aux/aux_data_generation.R")


# DATA SIMULATION ---------------------------------------------------------

task_config <- fromJSON(file = "data/task_config.json")

# Final data.frame
set.seed(2023)
tasks_data <- do.call(rbind, lapply(task_config, data_generation))

# SAVING DATA -------------------------------------------------------------

write.csv(tasks_data, file = "data/tasks_data.csv", row.names = FALSE)


