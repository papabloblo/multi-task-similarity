#'
#' Cálculo de la distancia de Frechet entre curvas
#' 
#' https://www.rdocumentation.org/packages/SimilarityMeasures/versions/1.4



# DEPENDENCIAS ------------------------------------------------------------

library(SimilarityMeasures)
library(longitudinalData)
library(tidyverse)


# Creating two trajectories.
path1 <- matrix(c(0, 1, 2, 3, 0, 1, 2, 3), 4)
path2 <- matrix(c(0, 1, 2, 3, 4, 5, 6, 7), 4)

# Running the Frechet distance algorithm.
Frechet(path1, path2)


# Visualization

df <- as.data.frame(path1)
df$id <- 1

df2 <- as.data.frame(path2)
df2$id <- 2

df <- rbind(df, df2)

ggplot(
  data = df,
  mapping = aes(x = V1, y = V2, group = id)
) +
  geom_line()


ale_by_var %>% 
  filter(.feature == "x4"
         # , task %in% c(2, 5)
         ) %>% 
  ggplot(
    mapping = aes(x = .borders, y = .value, group = task)
  ) +
  geom_line()

distances <- matrix(nrow = 5, ncol = 5)
distances2 <- matrix(nrow = 5, ncol = 5)

for (t1 in 1:4){
  for (t2 in (t1+1):5){
    x <- ale_by_var[ale_by_var$.feature == "x4" & ale_by_var$task == t1, c(".borders", ".value")]
    x <- as.matrix(x)
    
    y <- ale_by_var[ale_by_var$.feature == "x4" & ale_by_var$task == t2, c(".borders", ".value")]
    y <- as.matrix(y)
    
    distances[t2, t1] <- Frechet(x, y)
    distances2[t2, t1] <- distFrechet(x[,1], x[,2], y[,1], y[,2], FrechetSumOrMax = "sum")
  }
}
distances <- as.dist(distances)
x <- hclust(distances)
plot(x)

distFrechet()

ale_by_var %>% 
  filter(.feature == "x4"
         # , task %in% c(2, 5)
  ) %>% 
  mutate(clusters = case_when( task %in% c(2,5) ~ 1,
                               task %in% c(3, 4) ~ 2,
                               task %in% c(1) ~ 3)) %>% 
  ggplot(
    mapping = aes(x = .borders, y = .value, group = task)
  ) +
  geom_line() +
  facet_wrap(vars(clusters))

# Prueba

y <- x
y[,2] <- y[,2] + 2.575718

Frechet(x, y)
