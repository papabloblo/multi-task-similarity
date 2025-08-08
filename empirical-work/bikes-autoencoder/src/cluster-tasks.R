#'
#' GOAL: CLUSTER THE ALE CURVES USING THE MULTI-TASK SIMILARITY MEASURE
#' 


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(dbscan)
library(cluster)
library(patchwork)
library(ggdendro)


# CARGA DE DATOS ----------------------------------------------------------

similarity <- read_csv("empirical-work/bikes-autoencoder/data/similarity_tasks_matrix.csv")

similarity$similarity[similarity$task1 == similarity$task2] <- 0

similarity %>% arrange(similarity)




simi_task_features <- read_csv("empirical-work/bikes-autoencoder/data/similarity_tasks_features_matrix.csv")
ale <- read_csv("empirical-work/bikes-autoencoder/data/ales.csv")

similarity <- similarity %>% 
  pivot_wider(names_from = task2, values_from = similarity) %>%
  select(-task1)

# FUNCIONES ---------------------------------------------------------------

join_clusters <- function(ale, hclust, n_clusters){
  cut_clusters <- cutree(hclust, k = n_clusters)
  
  names_tasks <- as.numeric(names(cut_clusters))
  names_cluster <- paste('Cluster', cut_clusters)
  
  task_cluster <- tibble(task = names_tasks,
                         cluster = names_cluster)
  
  print(table(cut_clusters))
  return(left_join(filter(ale, task %in% names_tasks), task_cluster))
}

plot_ale_by_cluster_feature <- function(ale_clus, 
                                        feature, 
                                        clus=NULL, 
                                        tasks=NULL,
                                        by_cluster=TRUE,
                                        alpha=0.1,
                                        linetype_by_task=FALSE){
  if (is.null(clus)){
    clus <- unique(ale_clus$cluster)
  }
  
  if (is.null(tasks)){
    tasks <- unique(ale_clus$task)
  }
  p <- ale_clus %>% 
    select(-feature) %>% 
    filter(cluster %in% clus,
           task %in% tasks,
           feature == feature) %>%
    ggplot(aes(x=x, y=ale_centered, group=task)) +
    # geom_line(alpha=alpha) +
    # facet_wrap(cluster ~ feature2, scales = "fixed", ncol = 1) +
    labs(y="", x="") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 6),
      strip.background = element_rect(fill = "gray90", color = NA),
      legend.position = "none"
    )   
  if (linetype_by_task){
    p <- p + geom_line(aes(linetype=task), alpha=alpha)
  } else {
    p <- p + geom_line( alpha=alpha)
  }
  if (by_cluster){
    p <- p + facet_wrap(cluster ~ feature, scales = "fixed", ncol = 1)
  } else {
    p <- p + facet_wrap( .~ feature, scales = "fixed", ncol = 1)
  }
  
  
  return(p)
}


# DISTANCE MATRIX ---------------------------------------------------------

sim_matrix <- as.matrix(similarity)
diag(sim_matrix) <- 0
dist_matrix <- as.dist(sim_matrix)

max(dist_matrix)
min(dist_matrix)

# DIVISIVE CLUSTERING ------------------------------------------------

hclust_avg <- hclust(dist_matrix, method = 'ward.D')
plot(hclust_avg)
cut_clusters <- cutree(hclust_avg, k = 10)


dendro <- ggdendrogram(hclust_avg, rotate = FALSE, size = 2, theme_dendro=F) +
  theme_void()


ggsave(
  filename = "empirical-work/bikes/plots/dendrogram.png", 
  plot = dendro,
  width = 20,
  height = 20,
  dpi = "print",
  units = "cm"
)

ale$cluster <- NULL
ale <- join_clusters(ale, hclust_avg, 20)


p <- ale %>% 
  ggplot(aes(x = ale_x, y = ale_y, group = task)) +
  geom_line(alpha=.1) +
  facet_wrap(cluster ~ feature, scales = "free_x", ncol = 6)


ggsave(
  plot = p,
  filename = "empirical-work/bikes-autoencoder/plots/ales-clusters.png", 
  width = 10,
  height = 100,
  dpi = "print",
  units = "cm"
)

  

ale2 <- ale %>% 
  group_by(task, feature) %>% 
  mutate( ale_y = (ale_y - mean(ale_y))/sd(ale_y))

p1 <- ale %>% 
  filter(task == 147 | task == 264) %>%
  ggplot(aes(x = ale_x, y = ale_y, group = task, linetype = as.factor(task))) +
  geom_line() +
  facet_wrap(. ~ feature, scales = "free_x", ncol = 6)


p2 <- ale %>% 
  filter(task == 93 | task == 187) %>%
  ggplot(aes(x = ale_x, y = ale_y, group = task, linetype = as.factor(task))) +
  geom_line() +
  facet_wrap(. ~ feature, scales = "free_x", ncol = 6)

p1 + p2 + plot_layout(ncol = 1)

p3 <- ale2 %>% 
  filter(task == 11 | task == 140) %>%
  ggplot(aes(x = ale_x, y = ale_y, group = task, linetype = as.factor(task))) +
  geom_line() +
  facet_wrap(. ~ feature, scales = "free_x", ncol = 6)

p1 + p2 + p3 + plot_layout(ncol = 1)


features <- unique(ale$feature)
myplots <- lapply(
  features, 
  plot_ale_by_cluster_feature, 
  ale = ale,
  clus=c('Cluster 2', 'Cluster 3'))

myplots[features == 'Month'][[1]] <- myplots[features == 'Month'][[1]] + 
  scale_x_continuous(breaks=c(1,3,6,9,12))

myplots[features == 'Pressure'][[1]] <- myplots[features == 'Pressure'][[1]] + 
  scale_x_continuous(n.breaks = 3)

myplots[features == 'Radiation'][[1]] <- myplots[features == 'Radiation'][[1]] + 
  scale_x_continuous(n.breaks = 3)

p <- wrap_plots(myplots, ncol=length(myplots))


ggsave(
  filename = "empirical-work/bikes/plots/bikes-cluster-2-3.png", 
  plot = p,
  width = 40,
  height = 10,
  dpi = "print",
  units = "cm"
)

# CLUSTERS RANDOM FOREST

ale2 <- ale

ale2$ale_centered <- ale2$ale_centered + rnorm(nrow(ale2), mean = 0, sd = 0.0005)
features <- unique(ale2$feature2)
myplots <- lapply(
  features, 
  plot_ale_by_cluster_feature, 
  ale = ale2,
  clus=c( 'Cluster 2', 'Cluster 3'))

myplots[features == 'Month'][[1]] <- myplots[features == 'Month'][[1]] + 
  scale_x_continuous(breaks=c(1,3,6,9,12))

myplots[features == 'Pressure'][[1]] <- myplots[features == 'Pressure'][[1]] + 
  scale_x_continuous(n.breaks = 3)

myplots[features == 'Radiation'][[1]] <- myplots[features == 'Radiation'][[1]] + 
  scale_x_continuous(n.breaks = 3)

p <- wrap_plots(myplots, ncol=length(myplots))



ggsave(
  filename = "empirical-work/bikes/plots/bikes-cluster-2-3_random-forest.png", 
  plot = p,
  width = 40,
  height = 10,
  dpi = "print",
  units = "cm"
)


# MOST DISSIMILAR TASKS ---------------------------------------------------

which(sim_matrix == max(sim_matrix), arr.ind = TRUE)


features <- unique(ale$feature2)
myplots <- lapply(
  features, 
  plot_ale_by_cluster_feature, 
  ale = ale,
  tasks=c('Task 55', 'Task 259'),
  by_cluster=FALSE,
  alpha=.7,
  linetype_by_task=TRUE)

myplots[features == 'Month'][[1]] <- myplots[features == 'Month'][[1]] + 
  scale_x_continuous(breaks=c(1,3,6,9,12))

myplots[features == 'Pressure'][[1]] <- myplots[features == 'Pressure'][[1]] + 
  scale_x_continuous(n.breaks = 3)

myplots[features == 'Radiation'][[1]] <- myplots[features == 'Radiation'][[1]] + 
  scale_x_continuous(n.breaks = 3)

p <- wrap_plots(myplots, ncol=length(myplots)) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom") & labs(linetype='')


ggsave(
  filename = "empirical-work/bikes/plots/bikes-most-dissimilar.png", 
  plot = p,
  width = 40,
  height = 5.5,
  dpi = "print",
  units = "cm"
)



# SIMILARITY PLOT MATRIX --------------------------------------------------

similarity_t <- similarity %>% 
  mutate(task0=0:263) %>% 
  pivot_longer(cols=-task0,
               names_to='task1',
               values_to = 'similarity')


p <- similarity_t %>% 
  mutate(
    task0 = as.numeric(task0),
    task1 = as.numeric(task1)
  ) %>% 
  ggplot(
    aes(x = task0, 
        y = task1, 
        fill = similarity
    )
  ) + 
  geom_raster() + 
  theme_void() + 
  theme(
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 20, face = "bold"),
    legend.key.size = unit(1.5, "cm"),
    legend.text = element_text(size = 20)
  ) +
  labs(
    fill = "Multi-task\n similarity",
  ) +
  scale_x_continuous(
    breaks = c(0, 263),
    expand = expansion(0.01),
    position = "top"
  ) +
  scale_y_reverse(
    breaks = c(0, 263),
    expand = expansion(0.01)
  ) + 
  scale_fill_viridis_c(
    option = "A", 
    # colors ordered from lightest to darkest
    direction = -1
  )


ggsave(p, 
       file = "empirical-work/bikes/plots/bikes-matrix_similarity.png", 
       dpi = "print",
       width = 25,
       height = 20,
       units = "cm")

# NETWORK -----------------------------------------------------------------

library(igraph)
library(intergraph)
library(ggnetwork)
library(Matrix)

cut_clusters <- cutree(hclust_avg, k = 6)

clust <- cut_clusters %in% c(2)
sim_matrix2 <- sim_matrix[clust, clust]
diag(sim_matrix2) <- Inf
min_values <- apply(sim_matrix2, MARGIN=2, FUN=min)
which_min <- apply(sim_matrix2, MARGIN=2, FUN=which.min)

network_matrix <- Matrix(0, 
                         ncol=ncol(sim_matrix2), 
                         nrow=nrow(sim_matrix2),
                         sparse=TRUE)

for (i in 1:nrow(sim_matrix2)){
  # network_matrix[i, which_min[i]] <- min_values[i]
  network_matrix[i, which_min[i]] <- min_values[i]
  network_matrix[which_min[i], i] <- min_values[i]
}

a <- sim_matrix2[1:10,1:10]
a[1,5] <- 0
a[5,1] <- 0
g <- graph_from_adjacency_matrix(sim_matrix2, weighted = TRUE, mode='undirected')
plot(g)




ggplot(
 g,
  #can pass layout parameter
  aes(x, y, xend = xend, yend = yend)
)  +
  geom_edges(aes(alpha=weight)) +
  geom_nodes() 


g2 <- sample(
  x = 0:5, size = 100, replace = TRUE,
  prob = c(0.9, 0.02, 0.02, 0.02, 0.02, 0.02)
) %>%
  matrix(ncol = 10) %>%
  graph_from_adjacency_matrix(weighted = TRUE)

plot(g2)


E(g)$weight
pggnetwork=
  ggplot(
    ggnetwork(# provide the underlying dataframe
      g, #input: network object
      layout = "fruchtermanreingold",  #layout
      cell.jitter = 0.75),
    #can pass layout parameter
    aes(x, y, xend = xend, yend = yend)
  ) + #mapping for edges
  geom_edges(aes(linetype = as.factor(same.conf)),
             #arrow = arrow(length = unit(6, "pt"), type = "closed") #if directed
             color = "grey50",
             curvature = 0.2,
             alpha=0.5
  ) +
  geom_nodes(aes(color = conf), 
             size = 5,
             alpha=0.5) +
  scale_color_brewer("Conference",
                     palette = "Paired") +
  scale_linetype_manual(values = c(2, 1)) +
  guides(linetype = FALSE) +
  theme_blank()+ 
  geom_nodes(aes(color = conf), 
             size = 3) # can be treat as ggplot object and add geom_xx layer












g2 <- sample(
  x = 0:5, size = 100, replace = TRUE,
  prob = c(0.9, 0.02, 0.02, 0.02, 0.02, 0.02)
) %>%
  matrix(ncol = 10)
g1 <- sample(
  x = 0:1, size = 100, replace = TRUE,
  prob = c(0.9, 0.1)
) %>%
  matrix(ncol = 10) %>%
  graph_from_adjacency_matrix()


ale %>% 
  select(-feature) %>% 
  filter(
         x != 0) %>%
  ggplot(aes(x=x, y=ale_centered, group=task)) +
  geom_line(alpha=0.1) +
  facet_wrap(cluster ~ feature2, scales = "free", ncol = 10) +
  labs(y="", x="") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 6),
    strip.background = element_rect(fill = "gray90", color = NA),
    legend.position = "none"
  )   









# DBSCAN ------------------------------------------------------------------
dbscan(dist_matrix, 1, minPts = 5)
kNNdistplot(dist_matrix, k = 10)

dbscan(dist_matrix, eps=8, minPts = 100)

# CLUSTERING --------------------------------------------------------------

# Variable 6
mat <- simi_task_features %>% 
  select(task0, task1, var6 ) %>% 
  pivot_wider(names_from=task1, values_from=var6)


sim_matrix <- as.matrix(mat[,2:265])

dist_matrix <- as.dist(sim_matrix)

hclust_avg <- hclust(dist_matrix, method = 'average')


ale2 <- join_clusters(ale, hclust_avg, 30)



plot_ale_by_cluster(ale2, 1:4)


ale_clus %>% 
  filter(cluster%in%clus,
         x != 0) %>%
  ggplot(aes(x=x, y=ale, group=task)) +
  geom_line(alpha=0.1) +
  facet_wrap(cluster ~ feature, scales = "free_x", ncol = 10)





sim_matrix <- as.matrix(similarity)
min(sim_matrix)
diag(sim_matrix) <- 0
dist_matrix <- as.dist(sim_matrix)

hclust_avg <- hclust(dist_matrix, method = 'ward.D')
plot(hclust_avg)

ale2 <- join_clusters(ale, hclust_avg, 15)
plot_ale_by_cluster(ale2)
plot_ale_by_cluster(ale2, c(2,14,13))

cut_avg <- cutree(hclust_avg, k = 7)
# cut_avg <- cutree(as.hclust(diana_result), k = 10)

# cut_avg <- dbscan(dist_matrix, 1, minPts = 5)$cluster
ale2 <- join_clusters(ale, hclust_avg, 11)

plot_ale_by_cluster(ale2)

# Me quedo con 6 clusters.
# Vuelvo a dividir el cluster mayor

a <- cut_avg == 1

names(a)

sim_matrix2 <- sim_matrix[a,a]
dist_matrix2 <- as.dist(sim_matrix2)

dim(sim_matrix2)

hclust_avg <- hclust(dist_matrix2, method = 'average')
plot(hclust_avg)

ale2 <- join_clusters(ale, hclust_avg, 3)

plot_ale_by_cluster(ale2)
plot_ale_by_cluster(ale2, c(2,7,9,10))



table(cut_avg)
table(task_cluster$cluster)
table(ale$cluster)

table(ale$task)
length(unique(ale$task))



ale %>% 
  filter(task %in% c('task34', 'task123')) %>% 
  ggplot(aes(x=x, y=ale, group=task)) +
  geom_line() +
  facet_wrap(. ~ feature, scales = "free")


which.min(sim_matrix[1, 2:264])
which.max(sim_matrix[1, ])

ale %>% 
  filter(task %in% c('task0', 'task41')) %>% select(task, cluster) %>% distinct()


tasks <- task_cluster$task[task_cluster$cluster==1]

library(dbscan)

library(dbscan)
kNNdistplot(dist_matrix2, k = 4)  # If minPts = 5, use k = 4
abline(h = chosen_eps, col = "red", lty = 2)


# EDA SIMILARITY

a <- simi_task_features %>% 
  filter(task0 != task1)

a <- a %>% 
  mutate(dist = var0 + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9,
         across(var0:var9, ~(.x-mean(.x))/sd(.x)),
         dist_std = var0 + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9
         )


var4 = a$var4
mean(var4)
sd(var4)

var4 <- (var4-mean(var4))/sd(var4)

a %>% 
  filter(task0 == 0,
         task1 == 1)

skimr::skim(a)
similarity[1,2]

a %>% 
  arrange(desc(dist))

a %>% 
  arrange(desc(dist_std))


x <- c(2.2853,  1.8109,  1.4189,  1.5628,  1.3794,  1.4701,  1.5176,  0.9231,
0.5710,  0.0913, -0.6515, -0.6515, -0.6515, -0.6515, -0.6515, -0.6515,
-0.6515, -0.6515, -0.6515, -0.6515, -0.6515, -0.6515, -0.6515, -0.6515,
-0.6515, -0.6515, -0.6515, -0.6515, -0.6515, -0.6515)

sd(x)


# DIANA

diana_result <- diana(sim_matrix)
plot(diana_result)



# CLUSTER POR VARIABLE
# Variables "buenas" 6 y 8

mat <- simi_task_features %>% 
  select(task0, task1, var6 ) %>% 
  pivot_wider(names_from=task1, values_from=var6)


sim_matrix <- as.matrix(mat[,2:265])
dim(sim_matrix)
dist_matrix <- as.dist(sim_matrix)

hclust_avg <- hclust(dist_matrix, method = 'average')
plot(hclust_avg)



ale_clus <- join_clusters(ale, c)


