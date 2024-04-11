# Density of each variable to motivate the weighted version of Frechet distance



# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(ggridges)
library(patchwork)
library(latex2exp)

# DATA LOAD ---------------------------------------------------------------

df <- readRDS("empirical-work/synthetic-data-1/data/tasks_data.RDS") %>% 
  as_tibble()



# DENSITIES ---------------------------------------------------------------

df %>% 
  # filter(
  #   id_task == "1"
  # ) %>% 
  ggplot(
    aes(x = x1, group = id_task)
  ) +
  geom_density()



df %>% 
  # filter(
  #   id_task == "1"
  # ) %>% 
  ggplot(
    aes(x = x5, group = id_task)
  ) +
  geom_density()



p1 <- ggplot(df, aes(x = x4, y = id_task, group = id_task)) + 
  geom_density_ridges(fill = "firebrick") +
  theme_minimal()


p2 <- ggplot(df, aes(x = x5, y = id_task, group = id_task)) + 
  geom_density_ridges(fill = "firebrick") +
  theme_minimal()

p1 + p2


# p <- df %>% 
#   select(-y) %>% 
#   pivot_longer(!id_task, names_to = "var", values_to = "val") %>% 
#   mutate(id_task = paste("Task", id_task)) %>% 
#   ggplot(aes(x = val, y = id_task, group = id_task)) +
#   geom_density_ridges() +
#   facet_grid(cols = vars(var), scales = "free_x") +
#   labs(
#     y = "",
#     x = ""
#     ) +
#   theme_minimal() + 
#   theme(
#     panel.grid = element_blank(),
#     axis.text.x = element_blank())


p <- df %>% 
  select(-y) %>% 
  pivot_longer(!id_task, names_to = "var", values_to = "val") %>% 
  mutate(id_task = paste("Task", id_task),
         var = case_when(
           var == "x1" ~ TeX(r"($X_1$)", output = "character"),
           var == "x2" ~ TeX(r"($X_2$)", output = "character"),
           var == "x3" ~ TeX(r"($X_3$)", output = "character"),
           var == "x4" ~ TeX(r"($X_4$)", output = "character"),
           var == "x5" ~ TeX(r"($X_5$)", output = "character")
         )) %>% 
  ggplot(aes(x = val, y = id_task, group = id_task)) +
  geom_density_ridges() +
  facet_grid(~ var, 
             labeller = label_parsed, 
             scales = "free_x") +
  labs(
    y = "",
    x = ""
  ) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank())


ggsave("empirical-work/synthetic-data-1/plots/densities-synthetic1.png", 
       plot = p,
       dpi = "print",
       width = 20,
       height = 5,
       units = "cm")
