#' GOAL
#' 
#' Built a graph to motivate why to use sum 
#' instead of min in Fréchet distance
#' 


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
source("src/frechet/frechet.R")


# DATA GENERATION ---------------------------------------------------------

n <- 100



df <- tibble(
  x = seq(from = -5, to = 5, length.out = n),
  y1 = (x + 4)*x*(x - 4),
  y2 = 2*(x + 4)*x*(x - 4),
  y3 = (x + 4)*x*(x - 4) + 25,
  y4 = case_when(
   x < -4 ~ y2,
   x <= 4 ~ y1*1.2,
   x > 4 ~ y2
  ),
  
  )


p <- df %>% 
  ggplot(aes(x = x)) +
  geom_line(aes(y = y1), linewidth = 1.5) +
  # geom_line(aes(y = y2, color = "y2")) +
  geom_line(aes(y = y3), linetype = "dashed") +
  geom_line(aes(y = y4), linetype = "dotted") +
  geom_label(aes(x = 5, y = 45), label = 1) +
  geom_label(aes(x = 5, y = 68), label = 2) +
  geom_label(aes(x = 5, y = 85), label = 3) +
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) +
  theme_minimal() +
  labs(x = "X",
       y = "Y")


ggsave(p, file = "plots/example-sum-mi-frechet.png", 
       dpi = "print",
       width = 25,
       height = 10,
       units = "cm")


frechet2 <- function(y1, y2, SumOrMax){
  frechet(df$x, 
          y1, df$x, y2, n1 = 1, n2 = 1, fdist = euclid_dist, 
          fweight = function(a, b) 1, FrechetSumOrMax = SumOrMax)$Dist_frechet
}


# 1229.135
frechet2(df$y1, df$y2, SumOrMax = "sum")
# 45
frechet2(df$y1, df$y2, SumOrMax = "max")

# 1538.74
frechet2(df$y1, df$y3, SumOrMax = "sum")
# 25
frechet2(df$y1, df$y3, SumOrMax = "max")


# 338.1288
frechet2(df$y1, df$y4, SumOrMax = "sum")
# 45
frechet2(df$y1, df$y4, SumOrMax = "max")
