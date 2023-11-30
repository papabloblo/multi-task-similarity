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

df <- ale_t_x %>% 
  mutate(
    task = paste("Task", task),
    task2 = "1",
    n2 = n/1000,
    ymax = ale +  2*n2,
    ymin = ale -  2*n2) %>%
  bind_rows(mutate(ale_t_x,
                   task2 = task,
                   task = "All tasks"))

p1 <- df %>% 
  filter(feature == "x1") %>% 
  ggplot(aes(y = ale, x = x)) +
  geom_ribbon(aes(group = task2, ymax = ymax, ymin = ymin), alpha = 0.5) +
  geom_line(aes(group = task2)) +
  scale_y_continuous(limits = c(-4,4)) +
  facet_wrap(. ~ task + feature, ncol = 1) +
  labs(y = "",
       x = ""
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 6),
    panel.spacing.x = unit(0, "pt"),
    strip.background = element_rect(fill = "gray90", color = NA)
  )

p2 <- df %>% 
  filter(feature == "x2") %>% 
  ggplot(aes(y = ale, x = x)) +
  geom_ribbon(aes(group = task2, ymax = ymax, ymin = ymin), alpha = 0.5) +
  geom_line(aes(group = task2)) +
  scale_y_continuous(limits = c(-4,4)) +
  facet_wrap(. ~ task + feature, ncol = 1) +
  labs(y = "",
       x = ""
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 6),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "pt"),
    panel.spacing.x = unit(0, "pt"),
    strip.background = element_rect(fill = "gray90", color = NA)
  )

p3 <- df %>% 
  filter(feature == "x3") %>% 
  ggplot(aes(y = ale, x = x)) +
  geom_ribbon(aes(group = task2, ymax = ymax, ymin = ymin), alpha = 0.5) +
  geom_line(aes(group = task2)) +
  scale_y_continuous(limits = c(-4,4)) +
  facet_wrap(. ~ task + feature, ncol = 1) +
  labs(y = "",
       x = ""
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 6),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "pt"),
    panel.spacing.x = unit(0, "pt"),
    strip.background = element_rect(fill = "gray90", color = NA)
  )

p4 <- df %>% 
  filter(feature == "x4") %>% 
  ggplot(aes(y = ale, x = x)) +
  geom_ribbon(aes(group = task2, ymax = ymax, ymin = ymin), alpha = 0.5) +
  geom_line(aes(group = task2)) +
  scale_y_continuous(limits = c(-4,4)) +
  facet_wrap(. ~ task + feature, ncol = 1) +
  labs(y = "",
       x = ""
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 6),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "pt"),
    panel.spacing.x = unit(0, "pt"),
    strip.background = element_rect(fill = "gray90", color = NA)
  )
p5 <- df %>% 
  filter(feature == "x5") %>% 
  ggplot(aes(y = ale, x = x)) +
  geom_ribbon(aes(group = task2, ymax = ymax, ymin = ymin), alpha = 0.5) +
  geom_line(aes(group = task2)) +
  scale_y_continuous(limits = c(-4,4)) +
  facet_wrap(. ~ task + feature, ncol = 1) +
  labs(y = "",
       x = ""
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 6),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "pt"),
    panel.spacing.x = unit(0, "pt"),
    strip.background = element_rect(fill = "gray90", color = NA)
  )

p <- p1 + p2 + p3 + p4 + p5 + plot_layout(ncol = 5) & theme(plot.margin = margin(0, 0, 0, 0, "cm"))


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
       width = 25,
       height = 20,
       units = "cm")


# TODO --------------------------------------------------------------------


ale_t_x %>% 
  mutate(
    task = paste("Task:", task),
    task2 = "1",
    n2 = n/1000,
    ymax = ale +  2*n2,
    ymin = ale -  2*n2) %>%
  bind_rows(mutate(ale_t_x,
                   task2 = task,
                   task = "All tasks")) %>%
  ggplot(aes(y = ale, x = x)) +
  geom_ribbon(aes(group = task2, ymax = ymax, ymin = ymin), alpha = 0.5) +
  geom_line(aes(group = task2)) +
  facet_wrap(. ~ task + feature, scales = "free_x", ncol = 5) +
  labs(y = "",
       x = ""
  ) +
  theme(
    strip.text = element_text(size = 6)
  )



library(patchwork)
ale_t_x %>% 
  # filter(task  == 1) %>% 
  ggplot(aes(y = ale, x = x, group = task)) +
  geom_line(aes(linewidth = n)) +
  facet_wrap(. ~feature + task, scales = "free_x") +
  facet_wrap(. ~feature, scales = "free_x", ncol = 5) +
  labs(y = "", 
       x = "", 
       title = "",
       subtitle = ""
  )

p2 <- tasks_data %>%  
  filter(id_task == 1) %>% 
  mutate(id_task = paste("Task:", id_task)) %>% 
  dplyr::select(-y) %>% 
  pivot_longer(cols = !id_task) %>% 
  ggplot(aes(x = value, group = id_task)) +
  geom_histogram() +
  facet_wrap(. ~name, scales = "free", ncol = 5) +
  theme_void() +
  theme(strip.text = element_blank())
  

p3 <- ale_t_x %>% 
  filter(task  == 2) %>% 
  ggplot(aes(y = ale, x = x, group = task)) +
  geom_line() +
  # facet_wrap(. ~feature + task, scales = "free_x") +
  facet_wrap(. ~feature, scales = "free_x", ncol = 5) +
  labs(y = "", 
       x = "", 
       title = "",
       subtitle = ""
  )

p4 <- tasks_data %>%  
  filter(id_task == 2) %>% 
  mutate(id_task = paste("Task:", id_task)) %>% 
  dplyr::select(-y) %>% 
  pivot_longer(cols = !id_task) %>% 
  ggplot(aes(x = value, group = id_task)) +
  geom_histogram() +
  facet_wrap(. ~name, scales = "free", ncol = 5) +
  theme_void() +
  theme(strip.text = element_blank())


a <- p1 / p2 + plot_layout(ncol = 1, heights = c(5,1))
b <- p3 / p4 + plot_layout(ncol = 1, heights = c(5,1))

p1 / p2 / p1 / p2 /p1 / p2/ p1 / p2 / p3 / p4 + plot_layout(ncol = 1, heights = c(5,1,5,1, 5,1, 5,1, 5,1))

