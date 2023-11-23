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

df <- df %>% 
  group_by(id_task) %>% 
  mutate(across(x1:x5, ~(.x-mean(.x))/sd(.x))) %>% 
  ungroup()

saveRDS(df, "data/tasks_data_std.RDS")

# TRAINING AND ALE COMPUTATION --------------------------------------------

set.seed(2023)

ale_t_x <- ale_by_task_feature(df, randomForest)

saveRDS(ale_t_x, file = "data/ale_by_task_var.RDS")



  

p <- ale_t_x %>% 
  mutate(
    task = paste("Task:", task),
    task2 = "1") %>% 
  bind_rows(mutate(ale_t_x,
                   task2 = task,
                   task = "All tasks")) %>% 
  ggplot(aes(y = ale, x = x)) +
  geom_line(aes(group = task2)) +
  facet_wrap(. ~ task + feature, scales = "free_x", ncol = 5) +
  labs(y = "",
       x = ""
  ) +
  theme(
    strip.text = element_text(size = 6)
  )
  # labs(y = "f ale", 
  #      x = "", 
  #      title = "ALE plots by variable",
  #      subtitle = "Each line represents a task"
  # ) 

ggsave(p, file = "plots/ale_synthetic.png", 
       dpi = "print",
       width = 20,
       height = 25,
       units = "cm")

p2 <- ale_t_x %>% 
  ggplot(aes(y = ale, x = x, group = task)) +
  geom_line() +
  # facet_wrap(. ~feature + task, scales = "free_x") +
  facet_wrap(. ~feature, scales = "free_x", ncol = 5) +
  labs(y = "f ale", 
       x = "", 
       title = "ALE plots by variable",
       subtitle = "Each line represents a task"
  )

p1 / p2


