#' 
#' Create a plot of the weighted function used inside 
#' the weighted Frechet distance.


# DEPENDENCIES ------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(plotly)


# DATA GENERATION ---------------------------------------------------------

n <- 100
w <- seq(0.0001, 1, length.out = n)

w_function <- function(x, y) (max(x, y)/min(x, y))
w_function2 <- function(x, y) abs(x-y)

df <- expand.grid(x = w, y = w)
df$w <- mapply(w_function, df$x, df$y)
df$w2 <- abs(df$x - df$y)**2 + 1 

# PLOTS -------------------------------------------------------------------


ggplot(
  data = df,
  mapping = aes(
    x = x,
    y = y,
    fill = -w**2
  )
) +
  geom_raster()


df %>% 
  filter(x == df$x[2]) %>% 
ggplot(
  
  mapping = aes(
    x = y,
    y = w**2
  )
) +
  geom_line()



# volcano is a numeric matrix that ships with R

m <- matrix(data = df$w2, nrow = n, ncol = n)

fig <- plot_ly(z = ~m)
fig <- fig %>% add_surface()

fig


library(plot3D)
M <- mesh(seq(0, 6*pi, length.out = 50),seq(pi/3, pi, length.out = 50))
u <- M$x ; v <- M$y
x <- v * cos(u)
y <- v * sin(u)
z <- 10 * u
surf3D(as.matrix(df$x), as.matrix(df$y), as.matrix(df$w), colvar = as.matrix(df$w), colkey = TRUE, 
       box = TRUE, bty = "b", phi = 20, theta = 120)
