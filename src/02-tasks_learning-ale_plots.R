#' 
#' TRAINING THE MODELS AND ALE PLOTS COMPUTATION
#' 
#'  Output:
#'    - file: data/ale_by_task_var.RDS
#'    - description: A data.frame that contains the ALE curve
#'                  for each feature and task.
#' 
#'    


# DEPENDENCIES ------------------------------------------------------------
library(tidyverse)
library(randomForest)
library(ipred)
library(patchwork)

source("src/00-aux/ALE.R")


# READING DATA ----------------------------------------------------------

df <- read_csv("data/tasks_data.csv")

# Standardization

# df <- df %>% 
#   group_by(id_task) %>% 
#   mutate(across(x1:x5, ~(.x-mean(.x))/sd(.x))) %>% 
#   ungroup()

saveRDS(df, "data/tasks_data_std.RDS")

# TRAINING AND ALE COMPUTATION --------------------------------------------

set.seed(2023)

xALE <- grid_xALE(df, c("x1", "x2", "x3", "x4", "x5"), n = 50)

ale_t_x <- ale_by_task_feature(df, randomForest, xALE = xALE)


# SAVE  -------------------------------------------------------------------

saveRDS(ale_t_x, file = "data/ale_by_task_var.RDS")
