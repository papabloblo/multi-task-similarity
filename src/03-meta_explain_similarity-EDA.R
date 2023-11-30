#'
#' EXPLORATORY DATA ANALYSIS OF THE SIMILARITY RESULTS
#' 



# DEPENDENCIES ------------------------------------------------------------

library(kableExtra)
library(tidyverse)


# DATA LOADING ------------------------------------------------------------

task_feature <- readRDS("data/similarity/similarity_task_var_summary.RDS")
task <- readRDS("data/similarity/similarity_task_summary.RDS")


# DATA PREPARATION --------------------------------------------------------

task_feature <- task_feature %>% 
  mutate(task1 = paste("Task:", task1),
         task2 = paste("Task:", task2))

task <- task %>% 
  mutate(task1 = paste("Task:", task1),
         task2 = paste("Task:", task2))

df1 <- task_feature %>% 
  select(-f2, -importance, - wdist) %>% 
  pivot_wider(names_from = task2,
              values_from = dist_frechet)


df2 <- task_feature %>% 
  select(-task2, -f2, -dist_frechet, - wdist) %>%
  distinct()


df3 <- df1 %>% 
  left_join(df2, by = c("task1", "f1"))

df4 <- task %>% 
  pivot_wider(names_from = task2,
              values_from = similarity) %>% 
  mutate(f1 = "Similarity")


df <- bind_rows(df3, df4) %>% 
  arrange(task1, f1)


# LATEX TABLE -------------------------------------------------------------

df <- df %>% 
  select(task1, 
         f1, 
         "Task: 1", "Task: 2", "Task: 3", "Task: 4", "Task: 5",
         importance) 


df %>% 
  filter(f1 != "Similarity") %>% 
  group_by(task1) %>% 
  summarise(across(`Task: 1`:`Task: 5`, sum))

df %>% 
  mutate(
    across(where(is.numeric), ~round(.x, digits = 2)), 
    across(everything(), as.character),
    across(everything(), ~ifelse(is.na(.x), "-", .x))
  ) %>% 
  select(-task1) %>% 
  kbl(format = "latex", 
      align = "rcccccc",
      caption = "Group Rows", booktabs = T) %>% 
  kable_styling() %>%
  # column_spec(1, bold = TRUE) %>% 
  pack_rows(index = c("Task 1" = 6,
                      "Task 2" = 6,
                      "Task 3" = 6,
                      "Task 4" = 6,
                      "Task 5" = 6
                      ),
            latex_gap_space = "2em"
  )
