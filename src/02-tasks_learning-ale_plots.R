#' 
#' ENTRENAMIENTO DE LOS MODELOS Y GENERACIÓN DE LOS ALE PLOTS
#' 


# DEPENDENCIAS ------------------------------------------------------------
library(tidyverse)
library(randomForest)
library(iml)


# CARGA DE DATOS ----------------------------------------------------------

df <- read.csv("data/tasks_data.csv")


# ENTRENAMIENTO Y ALE PLOTS -----------------------------------------------

ale_plots <- function(df, method = 'ale'){
  mod <- randomForest(y ~ ., data = df)

  X <- df[names(df) != "y"]
  predictor <- Predictor$new(mod, data = X, y = df$y)
  
  return(FeatureEffects$new(predictor, method = method, grid.size = 30))
}


set.seed(2023)

list_ales <- list()
for (task in unique(df$id_task)){
  df_task <- df[df$id_task == task, ]
  df_task$id_task <- NULL
  list_ales[[task]] <- ale_plots(df_task)
}

ale_by_var <- map_df(1:length(list_ales), 
                     function(x){
                       y <- do.call(rbind, list_ales[[x]]$results)
                       y$task <- x
                       return(y)
                       }
                     )  

rownames(ale_by_var) <- NULL

ale_by_var$.type <- NULL

names(ale_by_var) <- c("y", "x", "feature", "task")

ale_by_var <-  select(.data = ale_by_var, 
                      task, feature, x, y )

ale_by_var <- as_tibble(ale_by_var)

saveRDS(ale_by_var, file = "data/ale_by_var.RDS")


ale_by_var %>% 
  filter(feature == "x1",
         task %in% c(1,2)) %>% 
  ggplot(aes(y = y, x = x, group = task)) +
  geom_line() +
  facet_wrap(. ~feature, scales = "free_x") +
  labs(y = "f ale", 
       x = "", 
       title = "ALE plots by variable",
       subtitle = "Each line represents a task"
  )
p <- ggplot(ale_by_var, aes(y = .value, x = .borders, group = task)) +
  geom_line() +
  facet_wrap(. ~.feature, scales = "free_x") +
  labs(y = "f ale", 
       x = "", 
       title = "ALE plots by variable",
       subtitle = "Each line represents a task"
       )

ggsave("plots/ale_by_var.png", plot = p, width = 30, height = 20, units = "cm")


