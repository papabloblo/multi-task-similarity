#'
#'    COMPUTE WEIGHTED FRECHET DISTANCE BETWEEN ALE CURVES
#'    



# DEPENDENCIES ------------------------------------------------------------
suppressMessages(library(R.utils, quietly = TRUE))
suppressMessages(library(tidyverse, quietly = TRUE))

source("src/frechet/frechet.R")

cmd <- cmdArgs()

# LOAD ALE CURVES ---------------------------------------------------------

ale_curves <- read_rds(cmd$ale_curves)
models <- readRDS(cmd$models)
# cmd <- list(same_features = FALSE)
# models <- readRDS("empirical-work/synthetic-data-2/data/models.RDS")
# ale_curves <- readRDS("empirical-work/synthetic-data-2/data/ale_by_task_var.RDS")
# FRECHET DISTANCE --------------------------------------------------------

# weight function for Frechet distance
fweight <- function(n1, n2) min(n1, n2)/max(n1, n2)

imp <- list()
for (task in names(models)){
  imp[[task]] <- models[[task]]$importance
}

# compute Frechet distance
frechet_results <- frechet_tasks_feature(ale_curves,
                                         importance = imp,
                                         fweight = fweight,
                                         same_features = cmd$same_features
                                         )

# summary of results
similarity_task_var_summary <- frechet_task_var_summary(frechet_results)

similarity_task_summary <- frechet_task_summary(frechet_results)


# DATA SAVING -------------------------------------------------------------

saveRDS(
  frechet_results, 
  cmd$similarity
)

saveRDS(
  similarity_task_var_summary, 
  cmd$task_var_similarity
  )

saveRDS(
  similarity_task_summary, 
  cmd$task_similarity
)


