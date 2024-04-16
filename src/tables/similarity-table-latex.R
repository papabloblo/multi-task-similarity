#'
#' EXPLORATORY DATA ANALYSIS OF THE SIMILARITY RESULTS
#' 



# DEPENDENCIES ------------------------------------------------------------

suppressMessages(library(R.utils, quietly = TRUE))
suppressMessages(library(tidyverse, quietly = TRUE))
suppressMessages(library(kableExtra))

source("src/frechet/frechet.R")

cmd <- cmdArgs()

# DATA LOADING ------------------------------------------------------------

task_feature <- readRDS(cmd$task_var_similarity)
task <- readRDS(cmd$task_similarity)


# task_feature <- readRDS("empirical-work/synthetic-data-1/data/similarity/similarity_task_var_summary.RDS")
# task <- readRDS("empirical-work/synthetic-data-1/data/similarity/similarity_task_summary.RDS")

# DATA PREPARATION --------------------------------------------------------

task_feature <- task_feature %>% 
  mutate(task1 = paste("Task", task1),
         task2 = paste("Task", task2),
         f1 = paste0("$", gsub("x", "X_", f1), "$"),
         f2 = paste0("$", gsub("x", "X_", f2), "$")) %>% 
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
  mutate(task1 = paste("Task", task1),
         task2 = paste("Task", task2)) %>% 
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
         "Task 1", "Task 2", "Task 3", "Task 4", "Task 5",
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
      caption = "Similarity measures between variables and tasks. The last column shows the feature importance.", 
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



# WORKING IN PROGRESS -----------------------------------------------------

sim_dist <- readRDS("empirical-work/synthetic-data-2/data/similarity/similarity.RDS")
sim_task <- readRDS("empirical-work/synthetic-data-2/data/similarity/similarity_task_summary.RDS")
df <- sim_task_feature_df(sim_dist)

df %>%
  group_by(task1, task2, f1) %>% 
  mutate(
    task1 = paste("Task", task1),
    task2 = paste("Task", task2),
    f1 = paste0("$", gsub("x", "X_", f1), "$"),
    f2 = paste0("$", gsub("x", "X_", f2), "$"),
    across(where(is.numeric), ~round(.x, digits = 2)),
    dist_frechet = ifelse(dist_frechet == min(dist_frechet),
                               cell_spec(dist_frechet, 
                                         format = "latex",
                                         bold = TRUE),
                               dist_frechet
                               )
         ) %>% 
  ungroup() %>% 
  select(-importance) %>% 
  filter(
    task1 == "Task 1"
    ) %>% 
  pivot_wider(names_from = f2,
              values_from = dist_frechet) %>% 
  arrange(task1, f1, task2) %>% 
  mutate(
    across(where(is.numeric), ~round(.x, digits = 2)), 
    across(everything(), as.character)
  ) %>% 
  select(-task1, -f1) %>% 
  kbl(format = "latex", 
      align = "rccccc",
      caption = "Group Rows", 
      booktabs = T, 
      escape = FALSE) %>% 
  kable_styling() %>%
  # column_spec(1, bold = TRUE) %>% 
  pack_rows(index = c("$X_1$" = 4,
                      "$X_2$" = 4,
                      "$X_3$" = 4,
                      "$X_4$" = 4,
                      "$X_5$" = 4
                      ),
            latex_gap_space = "2em",
            escape = FALSE
            )
  


sim_task %>% 
  group_by(task1) %>% 
  mutate(
    task1 = paste("Task", task1),
    task2 = paste("Task", task2),
    across(where(is.numeric), ~round(.x, digits = 2)),
    similarity = ifelse(similarity == min(similarity),
                          cell_spec(similarity, 
                                    format = "latex",
                                    bold = TRUE),
                        similarity
    )
    ) %>% 
  pivot_wider(names_from = task2,
              values_from = similarity) %>% 
  mutate(across(everything(), ~ifelse(is.na(.x), "-", .x))) %>% 
  select(task1, 
         "Task 1", "Task 2", "Task 3", "Task 4", "Task 5") %>% 
  kbl(format = "latex", 
      align = "r|ccccc",
      caption = "Group Rows", 
      booktabs = T, 
      escape = FALSE) %>% 
  kable_styling()
  
