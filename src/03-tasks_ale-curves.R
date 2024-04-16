#' 
#' TRAINING THE MODELS AND ALE PLOTS COMPUTATION
#' 
#'  Output:
#'    - file: data/ale_by_task_var.RDS
#'    - description: A data.frame that contains the ALE curve
#'                  for each feature and task.
#' 
#'    

suppressMessages(library(R.utils, quietly = TRUE))
cmd <- cmdArgs()

# DEPENDENCIES ------------------------------------------------------------
suppressMessages(library(tidyverse))
suppressMessages(library(randomForest))
source("src/00-aux/ALE.R")


# READING DATA ----------------------------------------------------------

df <- readRDS(cmd$tasks_data)
model <- readRDS(cmd$models)

# df <- readRDS("empirical-work/real-data/data/tasks_data.RDS")
# model <- readRDS("empirical-work/real-data/data/models.RDS")
# TRAINING AND ALE COMPUTATION --------------------------------------------

features <- setdiff(names(df), c(cmd$response, cmd$id_task))
# features <- c("x1", "x2", "x3", "x4", "x5")

xALE <- grid_xALE(df, features , n = cmd$n)


ale_t_x <- ale_by_task_feature(df, 
                               model =  model, 
                               xALE = xALE, 
                               features = features
                               )


# SAVE  -------------------------------------------------------------------

saveRDS(ale_t_x, file = cmd$out_file)
