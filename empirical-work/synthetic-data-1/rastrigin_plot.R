#' PLOTS OF RASTRIGIN FUNCTION



# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(plotly)

source("src/00-aux/aux_data_generation.R")



# DATA --------------------------------------------------------------------

x1 <- seq(from = -6, 
          to = 6,
          length.out = 1000)

x2 <- seq(from = -6, 
          to = 6,
          length.out = 1000)


df <- expand.grid(x1 = x1, x2 = x2)

df$z <- rastrigin(df$x1, df$x2)



# CONTOUR PLOT ------------------------------------------------------------

p <- ggplot(df, aes(x = x1, y = x2, z = z)) +
  geom_contour_filled() +
  geom_contour(color = "white") +
  theme_minimal()


# ggplot(df, aes(x = x1, y = x2, z = z)) +
#   geom_raster(aes(fill = z)) +
#   geom_contour(color = "white")


ggsave("empirical-work/synthetic-data-1/plots/rastrigin.png",
       plot = p,
       width = 20,
       height = 15,
       units = "cm",
       dpi = "print")



# 3D SURFACE PLOT ---------------------------------------------------------

z <- matrix(data = df$z, nrow = 1000, ncol = 1000)
fig <- plot_ly(x = x1, y = x2, z = z) %>% add_surface()
fig
