#'
#'    COMPUTE WEIGHTED FRECHET DISTANCE BETWEEN ALE CURVES
#'    



# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(randomForest)

source("src/frechet/frechet.R")

# LOAD ALE CURVES ---------------------------------------------------------

ale_curves <- read_rds("data/ale_by_task_var.RDS")
# df <- read_csv("data/tasks_data.csv")
df <- read_rds("data/tasks_data_std.RDS")


# FRECHET DISTANCE --------------------------------------------------------

euclid_dist <- function(x1, x2) sqrt(sum((x1-x2)**2))
fweight <- function(n1, n2) max(n1, n2)/min(n1, n2)


frechet_results <- frechet_taks_feature(ale_curves,
                                        fdist = euclid_dist,
                                        fweight = fweight)


frechet_task_feature_summary <- frechet_summary(frechet_results)

imp <- importance(df)

names(imp) <- 1:length(imp)
for (task in names(imp)){
  imp[[task]] <- data.frame(task = task, 
             feature = names(imp[[task]]), 
             imp = imp[[task]]
             )
  row.names(imp[[task]]) <- NULL
}
imp <- bind_rows(imp)


frechet_task_feature_summary <- frechet_task_feature_summary %>% 
  left_join(imp, by = c("task1" = "task",
                        "f1" = "feature")) %>% 
  rename(impf1 = imp) %>% 
  left_join(imp, by = c("task2" = "task",
                        "f2" = "feature")) %>% 
  rename(impf2 = imp) %>% 
  mutate(w_imp = mapply(fweight, n1 = impf1, n2 = impf2))



frechet_task_feature_summary <- frechet_task_feature_summary %>%
  mutate(dist_frechet2 = dist_frechet * w_imp)


frechet_task_feature_summary2 <- frechet_task_feature_summary %>% 
  filter(f1 == f2)

frechet_task_feature_summary2 <- frechet_task_feature_summary2 %>% 
  arrange(task1, f1, task2) %>% 
  select(task1, task2, f1, dist_frechet2)



frechet_task_feature_summary2 %>% 
  pivot_wider(names_from = f1, values_from= dist_frechet2)



frechet_task_feature_summary %>% 
  filter(task1 %in% 1:2,
         task2 %in% 1:2,
         f1 %in% c("x1", "x2"),
         f2 %in% c("x1", "x2"))

frechet_task_feature_summary %>% 
  group_by(task1, task2, f1) %>% 
  filter(dist_frechet2 == min(dist_frechet2)) %>% 
  group_by(task1, task2) %>% 
  summarise(sum_frechet = sum(dist_frechet2)) %>% 
  arrange(task1, sum_frechet)


ale_curves %>% 
  ggplot(aes(y = ale, x = x, group = task)) +
  geom_line() +
  facet_wrap(. ~task + feature, scales = "free_x") +
  labs(y = "f ale", 
       x = "", 
       title = "ALE plots by variable",
       subtitle = "Each line represents a task"
  )

