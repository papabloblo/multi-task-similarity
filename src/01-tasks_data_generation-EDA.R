#' 
#' Exploratory data analysis per task
#' 


library(tidyverse)
library(latex2exp)


# names(tasks_data)[1:5] <- TeX(paste0("X_", 1:5))


p <- tasks_data %>% 
  mutate(id_task = paste("Task:", id_task)) %>% 
  select(-y) %>% 
  pivot_longer(cols = !id_task) %>% 
  # mutate(name = TeX(paste0(name, "_7"))) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() +
  facet_wrap(vars(id_task, name),
             scales = "free", nrow = 5) +
  # facet_grid(rows = vars(name),
  #            cols = vars(id_task),
  #            scales = "free_x") +
  labs(x = "") +
  # theme_bw() +
  scale_x_continuous(
    labels = function(x) round(x, 1),
    n.breaks = 3) +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 6),
    panel.grid = element_blank(),
    strip.text = element_text(size = 6),
    panel.background = element_blank()
  )

ggsave(p, file = "plots/features_distribution.png", 
       dpi = "print",
       width = 14,
       height = 14,
       units = "cm")

tasks_data %>% 
  pivot_longer(cols = !id_task) %>% 
  group_by(id_task, name) %>% 
  summarise(a = mean(value),
            b = sd(value))


tasks_data %>% 
  filter(id_task == 2) %>% 
  ggplot(aes(x=x1, y=x2)) +
  geom_point(alpha = .1)



tasks_data %>% 
  filter(id_task == 4) %>% 
  ggplot(aes(x=x4, y=y)) +
  geom_point(alpha = .1)

tasks_data %>% 
  filter(id_task == 3) %>% 
  ggplot(aes(x=x4, y=y2)) +
  geom_point(alpha = .1)


tasks_data$y2 <- std(tasks_data$x4**2 + tasks_data$x5**2 + tasks_data$x4*df$x5) 

hist(rsn(n = 1000, omega = 1, alpha = -2))
