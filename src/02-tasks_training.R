#'
#' TASKS TRAINING
#' 


# DEPENDENCIES ------------------------------------------------------------

suppressMessages(library(R.utils, quietly = TRUE))
suppressMessages(library(tidyverse, quietly = TRUE))
suppressMessages(library(randomForest, quietly = TRUE))

cmd <- cmdArgs()


# READING DATA ----------------------------------------------------------

df <- readRDS(cmd$tasks_data)

if ("seed" %in% names(cmd)){
  set.seed(cmd$seed)
}


list_importance <- list()
for (task in unique(df$id_task)){
  cat("Task", task, "of", length(unique(df$id_task)),"\n")
        
  df_task <- df[df$id_task == task, ]
  df_task$id_task <- NULL
  
  if (identical(cmd$predictors, "all")){
    predictors <- names(df_task)[names(df_task) != cmd$response]
  } else{
    predictors <- cmd$predictors
  }
    
  
  mod <- randomForest(
    reformulate(response = cmd$response, termlabels = predictors),
    data = df_task,
    importance = TRUE
  )
  
  imp <- mod$importance[, "IncNodePurity"]
  
  list_importance[[task]] <- list(model = mod,
                                  importance = imp/sum(imp)
                                  )
}


# SAVE LIST OF MODELS -----------------------------------------------------

saveRDS(list_importance, file = cmd$output)
