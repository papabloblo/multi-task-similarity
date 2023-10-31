#' 
#' TRAINING THE MODELS AND ALE PLOTS COMPUTATION
#' 
#'  Output:
#'    - file: data/ale_by_task_var.RDS
#'    - description: A data.frame that contains the ALE curve
#'                  for each feature and task.
#' 
#' TODO:
#'    1. Abstract model function in ale_plots.
#'    2. Move functions to aux.R file.
#'    


# DEPENDENCIES ------------------------------------------------------------
library(tidyverse)
library(randomForest)

source("src/00-aux/ALE.R")


# READING DATA ----------------------------------------------------------

df <- read_csv("data/tasks_data.csv")


# TRAINING AND ALE COMPUTATION --------------------------------------------

ale_plots <- function(df, response = "y", predictors = "all"){
  
  if (predictors[1] == "all") 
    predictors <- names(df)[names(df) != response]
  
  mod <- randomForest(
    reformulate(response = "y", termlabels = predictors),
    data = df
    )
  
  return(ale(df, mod, features = predictors))
}


set.seed(2023)

list_ales <- list()
for (task in unique(df$id_task)){
  df_task <- df[df$id_task == task, ]
  df_task$id_task <- NULL
  list_ales[[task]] <- ale_plots(df_task)
}


if (is.null(names(list_ales))) names(list_ales) <- 1:length(list_ales)

aux <- list()
for (task in names(list_ales)){
  for (x in names(list_ales[[task]])){
    list_ales[[task]][[x]]$task <- task
    list_ales[[task]][[x]]$feature <- x
  }
  aux[[task]] <- bind_rows(list_ales[[task]])
}

ale_by_task_var <- bind_rows(aux)

saveRDS(ale_by_task_var, file = "data/ale_by_task_var.RDS")


ale_by_task_var %>% 
  ggplot(aes(y = ale, x = x, group = task)) +
  geom_line() +
  facet_wrap(. ~feature, scales = "free_x") +
  labs(y = "f ale", 
       x = "", 
       title = "ALE plots by variable",
       subtitle = "Each line represents a task"
  )


