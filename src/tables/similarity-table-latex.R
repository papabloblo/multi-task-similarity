#'
#' EXPLORATORY DATA ANALYSIS OF THE SIMILARITY RESULTS
#' 



# DEPENDENCIES ------------------------------------------------------------

suppressMessages(library(R.utils, quietly = TRUE))
suppressMessages(library(tidyverse, quietly = TRUE))
suppressMessages(library(kableExtra))

cmd <- cmdArgs()

# DATA LOADING ------------------------------------------------------------

task_feature <- readRDS(cmd$task_var_similarity)
task <- readRDS(cmd$task_similarity)


# task_feature <- readRDS("data/similarity/similarity_task_var_summary.RDS")
# task <- readRDS("data/similarity/similarity_task_summary.RDS")

# DATA PREPARATION --------------------------------------------------------

task_feature <- task_feature %>% 
  mutate(task1 = paste("Task:", task1),
         task2 = paste("Task:", task2)) %>% 
  mutate(
    across(where(is.numeric), ~round(.x, digits = 2))) %>% 
  group_by(task1, f1) %>% 
  mutate(dist_frechet = ifelse(dist_frechet == min(dist_frechet),
                               cell_spec(dist_frechet, 
                                         format = "latex",
                                         bold = TRUE),
                               dist_frechet)) %>% 
  ungroup()

task <- task %>% 
  mutate(task1 = paste("Task:", task1),
         task2 = paste("Task:", task2)) %>% 
  mutate(
    across(where(is.numeric), ~round(.x, digits = 2))) %>% 
  group_by(task1) %>% 
  mutate(similarity = ifelse(similarity == min(similarity),
                               cell_spec(similarity, 
                                         format = "latex", 
                                         bold = TRUE),
                               similarity))

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
  mutate(
    across(where(is.numeric), ~round(.x, digits = 2)), 
    across(everything(), as.character),
    across(everything(), ~ifelse(is.na(.x), "-", .x))
  ) %>% 
  select(-task1) %>% 
  kbl(format = "latex", 
      align = "rcccccc",
      caption = "Group Rows", 
      booktabs = T, 
      escape = FALSE) %>% 
  kable_styling() %>%
  # column_spec(1, bold = TRUE) %>% 
  pack_rows(index = c("Task 1" = 6,
                      "Task 2" = 6,
                      "Task 3" = 6,
                      "Task 4" = 6,
                      "Task 5" = 6
                      ),
            latex_gap_space = "2em"
  ) %>% 
  write_file(file = cmd$out_table)
