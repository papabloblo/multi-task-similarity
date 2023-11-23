#'
#'
#' EXPLORATORY DATA ANALYSIS FOR ALE CURVES
#' 
#' 


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)


# READING DATA ------------------------------------------------------------

ale_t_x <- readRDS(file = "data/ale_by_task_var.RDS")


# PLOTS -------------------------------------------------------------------


# ALE curves by variable and task
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

# ALE curves by variable (all curves grouped by var)
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


# SAVING PLOTS ------------------------------------------------------------

ggsave(p, file = "plots/ale_synthetic.png", 
       dpi = "print",
       width = 20,
       height = 25,
       units = "cm")


