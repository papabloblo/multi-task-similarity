# Density of each variable to motivate the weighted version of Frechet distance



# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(ggridges)
library(patchwork)

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


df %>% 
  select(-y) %>% 
  pivot_longer(!id_task, names_to = "var", values_to = "val") %>% 
  mutate(id_task = paste("Task", id_task)) %>% 
  ggplot(aes(x = val, y = id_task, group = id_task)) +
  geom_density_ridges() +
  facet_grid(cols = vars(var), scales = "free_x") +
  labs(
    y = "",
    x = ""
    ) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank())
