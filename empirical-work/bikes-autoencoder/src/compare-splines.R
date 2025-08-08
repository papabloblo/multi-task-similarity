# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(dbscan)
library(cluster)
library(patchwork)
library(ggdendro)


# CARGA DE DATOS ----------------------------------------------------------

sim_orig <- read_csv("empirical-work/bikes-autoencoder/data/similarity_tasks_matrix-orig.csv")
sim_splines <- read_csv("empirical-work/bikes-autoencoder/data/similarity_tasks_matrix-spline.csv")

sim_orig <- sim_orig %>% 
  rename(sim_orig = similarity)

sim_splines <- sim_splines %>% 
  rename(sim_splines = similarity)


similarities <- sim_orig %>% left_join(sim_splines)

similarities <- similarities %>% 
  filter(task1 != task2) %>% 
  mutate(dif = sim_orig - sim_splines) %>% 
  filter(!is.na(dif))

similarities %>% 
  ggplot(aes(x = sim_orig, y = sim_splines)) +
  geom_point()

sqrt(mean(similarities$dif**2))

similarities %>% 
  ggplot(aes(x = dif)) +
  geom_histogram()

summary(similarities$sim_orig)
summary(similarities$sim_splines)

sd(similarities$sim_orig)
sd(similarities$sim_splines)

similarities %>% 
  ggplot(aes(x = sim_orig)) +
  geom_histogram()

cor(similarities$sim_orig, similarities$sim_splines, method='spearman')
