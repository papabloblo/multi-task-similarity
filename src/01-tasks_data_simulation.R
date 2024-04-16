#' 
#' DATA SIMULATION FOR TASKS
#' 
#' 





# DEPENDENCIES ------------------------------------------------------------

suppressMessages(library(R.utils, verbose = FALSE))
suppressMessages(library(rjson, verbose = FALSE))
suppressMessages(library(tidyverse, verbose = FALSE))

source("src/00-aux/aux_data_generation.R")

cmd <- cmdArgs()

# DATA CONFIG -------------------------------------------------------------

task_config <- fromJSON(file = cmd$task_config)

# Required packages for simulation
for (pack in task_config$packages){
  suppressMessages(library(pack, character.only = TRUE))
}



# DATA SIMULATION ---------------------------------------------------------

if ("seed" %in% names(cmd)){
  set.seed(cmd$seed)  
}

# Final data.frame
tasks_data <- do.call(rbind, lapply(task_config$tasks, data_generation))

# SAVING DATA -------------------------------------------------------------

saveRDS(tasks_data, file = cmd$output)
