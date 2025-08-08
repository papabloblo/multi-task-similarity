# GOAL: compare the most similar and dissimilar tasks with and without autoencoder


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(dbscan)
library(cluster)
library(patchwork)
library(ggdendro)


# CARGA DE DATOS ----------------------------------------------------------

sim_auto <- read_csv("empirical-work/bikes-autoencoder/data/similarity_tasks_matrix.csv")
sim_orig <- read_csv("empirical-work/bikes-autoencoder/data/similarity_tasks_matrix-orig.csv")

ale_auto <- read_csv("empirical-work/bikes-autoencoder/data/ales.csv")
ale_orig <- read_csv("empirical-work/bikes-autoencoder/data/ales-orig.csv")

# Name of variables

ale_auto <- ale_auto %>% 
  mutate(feature = paste('Latent feature ', feature + 1))

ale_orig <- ale_orig %>% 
  mutate(
    feature = case_when(
      feature == 0 ~ 'Lag 1',
      feature == 1 ~ 'Lag 2',
      feature == 2 ~ 'Lag 3',
      feature == 3 ~ 'Lag 4',
      feature == 4 ~ 'Lag 5',
      feature == 5 ~ 'Wind speed',
      feature == 6 ~ 'Wind direction',
      feature == 7 ~ 'Temperature',
      feature == 8 ~ 'Humidity',
      feature == 9 ~ 'Pressure',
      feature == 10 ~ 'Radiation',
      feature == 11 ~ 'Precipitation',
      feature == 12 ~ 'Holiday',
      feature == 13 ~ 'Month',
      feature == 14 ~ 'Hour',
      feature == 15 ~ 'Weekday'
    )
  )


# Remove similarities between same tasks
sim_auto <- sim_auto[sim_auto$task1 != sim_auto$task2,]
sim_orig <- sim_orig[sim_orig$task1 != sim_orig$task2,]

sim_auto <- na.omit(sim_auto)
sim_orig <- na.omit(sim_orig)

sim_orig <- sim_orig[!is.infinite(sim_orig$similarity),]

# Most similar tasks
sim_auto[sim_auto$similarity == min(sim_auto$similarity), ]
sim_orig[sim_orig$similarity == min(sim_orig$similarity), ]

# Most dissimilar tasks
sim_auto[sim_auto$similarity == max(sim_auto$similarity), ]
sim_orig[sim_orig$similarity == max(sim_orig$similarity), ]


# PLOTS -------------------------------------------------------------------

ale_auto_most_sim <- ale_auto %>% 
  filter(task == 173 | task == 215) %>%
  # filter(task == 93 | task == 187) %>%
  ggplot(aes(x = ale_x, y = ale_y, group = task, linetype = as.factor(task))) +
  geom_line() +
  scale_x_continuous(n.breaks = 3) +
  facet_wrap(. ~ feature, scales = "free", nrow = 1) +
  labs(y="", x="", linetype = 'Unlocked station (task)') +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),
    strip.background = element_rect(fill = "gray90", color = NA),
    legend.position = "bottom"
  )   

ale_orig_most_sim <- ale_orig %>% 
  filter(task == 173 | task == 215) %>%
  filter(!feature %in% c('Lag 2', 'Lag 3', 'Lag 4', 'Lag 5')) %>% 
  ggplot(aes(x = ale_x, y = ale_y, group = task, linetype = as.factor(task))) +
  geom_line() +
  scale_x_continuous(n.breaks = 3) +
  facet_wrap(. ~ feature, scales = "free", nrow = 2)+
  labs(y="", x="", linetype = 'Unlocked station (task)') +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),
    strip.background = element_rect(fill = "gray90", color = NA),
    legend.position = "none"
  )   


p <- ale_orig_most_sim + ale_auto_most_sim + plot_layout(ncol = 1)

ggsave(
  plot = p,
  filename = "empirical-work/bikes-autoencoder/plots/bikes-most-similar.png", 
  width = 40,
  height = 20,
  dpi = "print",
  units = "cm"
)

ale_auto_less_sim <- ale_auto %>% 
  # filter(task == 147 | task == 264) %>%
  filter(task == 138 | task == 215) %>%
  ggplot(aes(x = ale_x, y = ale_y, group = task, linetype = as.factor(task))) +
  geom_line() +
  scale_x_continuous(n.breaks = 3) +
  facet_wrap(. ~ feature, scales = "free", nrow = 1)+
  labs(y="", x="", linetype = 'Unlocked station (task)') +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),
    strip.background = element_rect(fill = "gray90", color = NA),
    legend.position = "bottom"
  )   

ale_orig_less_sim <- ale_orig %>% 
  filter(task == 138 | task == 215) %>%
  filter(!feature %in% c('Lag 2', 'Lag 3', 'Lag 4', 'Lag 5')) %>% 
  ggplot(aes(x = ale_x, y = ale_y, group = task, linetype = as.factor(task))) +
  geom_line() +
  scale_x_continuous(n.breaks = 3) +
  facet_wrap(. ~ feature, scales = "free", nrow = 2)+
  labs(y="", x="", linetype = 'Unlocked station (task)') +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),
    strip.background = element_rect(fill = "gray90", color = NA),
    legend.position = "none"
  )   


p <- ale_orig_less_sim + ale_auto_less_sim + plot_layout(ncol = 1)

ggsave(
  plot = p,
  filename = "empirical-work/bikes-autoencoder/plots/bikes-most-dissimilar.png", 
  width = 40,
  height = 20,
  dpi = "print",
  units = "cm"
)

ale_orig %>% 
  ggplot(aes(x = ale_x, y = ale_y, group = task)) +
  geom_line(alpha=.1) +
  facet_wrap( ~ feature, scales = "free", ncol = 6)



