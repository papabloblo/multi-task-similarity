#'
#'    COMPUTE WEIGHTED FRECHET DISTANCE BETWEEN ALE CURVES
#'    



# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(randomForest)
library(kableExtra)

source("src/frechet/frechet.R")

# LOAD ALE CURVES ---------------------------------------------------------

ale_curves <- read_rds("data/ale_by_task_var.RDS")
df <- read_csv("data/tasks_data.csv")
imp <- readRDS("data/importance.RDS")


# FRECHET DISTANCE --------------------------------------------------------

# weight function for Frechet distance
fweight <- function(n1, n2) min(n1, n2)/max(n1, n2)

# compute Frechet distance
frechet_results <- frechet_tasks_feature(ale_curves,
                                         importance = imp,
                                         fweight = fweight
                                         )

# summary of results
similarity_task_var_summary <- frechet_task_var_summary(frechet_results)

similarity_task_summary <- frechet_task_summary(frechet_results)


# DATA SAVING -------------------------------------------------------------

saveRDS(
  similarity_task_var_summary, 
  "data/similarity/similarity_task_var_summary.RDS"
  )

saveRDS(
  similarity_task_summary, 
  "data/similarity/similarity_task_summary.RDS"
)


