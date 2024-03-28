#' PLOTS FOR THE RELATION OF THE OUTCOME WITH THE VARIABLES



# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(plotly)
library(processx)

source("src/00-aux/aux_data_generation.R")




# RASTRIGIN FUNCTION ------------------------------------------------------


## DATA --------------------------------------------------------------------

n <- 1000
x1 <- seq(from = -6, 
          to = 6,
          length.out = n)

x2 <- seq(from = -6, 
          to = 6,
          length.out = n)


df <- expand.grid(x1 = x1, x2 = x2)

df$z <- rastrigin(df$x1, df$x2)



## CONTOUR PLOT ------------------------------------------------------------

p <- ggplot(df, aes(x = x1, y = x2, z = z)) +
  geom_contour_filled() +
  geom_contour(color = "white") +
  theme_minimal()


# ggplot(df, aes(x = x1, y = x2, z = z)) +
#   geom_raster(aes(fill = z)) +
#   geom_contour(color = "white")


ggsave("empirical-work/synthetic-data-1/plots/rastrigin-contour.png",
       plot = p,
       width = 20,
       height = 15,
       units = "cm",
       dpi = "print")



## 3D SURFACE PLOT ---------------------------------------------------------

z <- matrix(data = df$z, nrow = 1000, ncol = 1000)
fig <- plot_ly(x = x1, y = x2, z = z) %>% add_surface()
fig

save_image(fig, 
     file = "empirical-work/synthetic-data-1/plots/rastrigin-surface.png",
     scale = 3
     )


# install.packages('reticulate')
reticulate::install_miniconda()
reticulate::conda_install('r-reticulate', 'python-kaleido')
reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
reticulate::use_miniconda('r-reticulate')




# QUADRATIC FORM ----------------------------------------------------------

params <- list(
  task1 = list(a = 1, b = 1, c = 1),
  task2 = list(a = -1, b = -1, c = -1),
  task3 = list(a = 1, b = 1, c = -1),
  task4 = list(a = 1, b = 1, c = -1),
  task5 = list(a = -1, b = -1, c = -1)
)


params$task1
n <- 1000
x1 <- seq(-.5, .5, length.out = n)
x2 <- seq(-.5, .5, length.out = n)

tf1 <- expand.grid(x1 = x1, x2 = x2)
tf2 <- expand.grid(x1 = x1, x2 = x2)
tf3 <- expand.grid(x1 = x1, x2 = x2)
tf4 <- expand.grid(x1 = x1, x2 = x2)
tf5 <- expand.grid(x1 = x1, x2 = x2)

tf1$y <-  tf1$x1**2 + tf1$x2**2 + tf1$x1*tf1$x2
tf2$y <-  tf2$x1**2 + tf2$x2**2 - tf2$x1*tf2$x2
tf3$y <-  tf3$x1**2 - tf3$x2**2 + tf3$x1*tf3$x2
tf4$y <- -tf4$x1**2 + tf4$x2**2 + tf4$x1*tf4$x2
tf5$y <- -tf5$x1**2 - tf5$x2**2 + tf5$x1*tf5$x2



## CONTOUR PLOT ------------------------------------------------------------


p <- ggplot(tf1, aes(x = x1, y = x2, z = y)) +
  geom_contour_filled() +
  geom_contour(color = "white") +
  theme_minimal()


ggsave("empirical-work/synthetic-data-1/plots/quadratic-contour-task1.png",
       plot = p,
       width = 20,
       height = 15,
       units = "cm",
       dpi = "print")


p <- ggplot(tf2, aes(x = x1, y = x2, z = y)) +
  geom_contour_filled() +
  geom_contour(color = "white") +
  theme_minimal()


ggsave("empirical-work/synthetic-data-1/plots/quadratic-contour-task2.png",
       plot = p,
       width = 20,
       height = 15,
       units = "cm",
       dpi = "print")

p <- ggplot(tf3, aes(x = x1, y = x2, z = y)) +
  geom_contour_filled() +
  geom_contour(color = "white") +
  theme_minimal()


ggsave("empirical-work/synthetic-data-1/plots/quadratic-contour-task3.png",
       plot = p,
       width = 20,
       height = 15,
       units = "cm",
       dpi = "print")

p <- ggplot(tf4, aes(x = x1, y = x2, z = y)) +
  geom_contour_filled() +
  geom_contour(color = "white") +
  theme_minimal()


ggsave("empirical-work/synthetic-data-1/plots/quadratic-contour-task4.png",
       plot = p,
       width = 20,
       height = 15,
       units = "cm",
       dpi = "print")

p <- ggplot(tf5, aes(x = x1, y = x2, z = y)) +
  geom_contour_filled() +
  geom_contour(color = "white") +
  theme_minimal()


ggsave("empirical-work/synthetic-data-1/plots/quadratic-contour-task5.png",
       plot = p,
       width = 20,
       height = 15,
       units = "cm",
       dpi = "print")




z <- matrix(data = tf1$y, nrow = n, ncol = n)
fig <- plot_ly(x = x1, y = x2, z = z) %>% add_surface()
fig

save_image(fig, 
           file = "empirical-work/synthetic-data-1/plots/quadratic-task1.png",
           scale = 3
           )


z <- matrix(data = tf2$y, nrow = n, ncol = n)
fig <- plot_ly(x = x1, y = x2, z = z) %>% add_surface()
fig

save_image(fig, 
           file = "empirical-work/synthetic-data-1/plots/quadratic-task2.png",
           scale = 3
           )

z <- matrix(data = tf3$y, nrow = n, ncol = n)
fig <- plot_ly(x = x1, y = x2, z = z) %>% add_surface()


save_image(fig, 
           file = "empirical-work/synthetic-data-1/plots/quadratic-task3.png",
           scale = 3
)

z <- matrix(data = tf4$y, nrow = n, ncol = n)
fig <- plot_ly(x = x1, y = x2, z = z) %>% add_surface()


save_image(fig, 
           file = "empirical-work/synthetic-data-1/plots/quadratic-task4.png",
           scale = 3
)


z <- matrix(data = tf5$y, nrow = n, ncol = n)
fig <- plot_ly(x = x1, y = x2, z = z) %>% add_surface()


save_image(fig, 
           file = "empirical-work/synthetic-data-1/plots/quadratic-task5.png",
           scale = 3
)
