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

similarity <- read_csv("empirical-work/bikes-autoencoder/data/similarity_tasks_matrix-orig.csv")

ale <- read_csv("empirical-work/bikes-autoencoder/data/ales-orig.csv")

similarity <- similarity %>% 
  pivot_wider(names_from = task2, values_from = similarity) %>%
  select(-task1)



ale <- ale %>% 
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

sim_matrix[is.na(sim_matrix)] <- max(sim_matrix[is.finite(sim_matrix)], na.rm = TRUE)
sim_matrix[is.infinite(sim_matrix)] <- max(sim_matrix[is.finite(sim_matrix)], na.rm = TRUE)
diag(sim_matrix) <- 0
dist_matrix <- as.dist(sim_matrix)

max(dist_matrix)
min(dist_matrix)

# DIVISIVE CLUSTERING ------------------------------------------------

hclust_avg <- hclust(dist_matrix, method = 'ward.D')



dendro <- ggdendrogram(hclust_avg, rotate = FALSE, size = 2, theme_dendro=F) +
  theme_void()


ggsave(
  filename = "empirical-work/bikes-autoencoder/plots/dendrogram.png", 
  plot = dendro,
  width = 20,
  height = 20,
  dpi = "print",
  units = "cm"
)

ale$cluster <- NULL
ale <- join_clusters(ale, hclust_avg, 50)


p <- ale %>% 
  filter(cluster %in% c('Cluster 27', 'Cluster 33')) %>% 
  mutate(cluster = case_when(
    cluster == 'Cluster 27' ~ 'Cluster 1',
    cluster == 'Cluster 33' ~ 'Cluster 3',
  )) %>% 
  filter(!feature %in% c('Lag 2', 'Lag 3', 'Lag 4', 'Lag 5')) %>% 
  ggplot(aes(x = ale_x, y = ale_y, group = task)) +
  geom_line(alpha=.4) +
  scale_x_continuous(n.breaks = 3) +
  facet_wrap(feature ~ cluster, scales = "free", ncol = 6) +
  labs(y="", x="") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),
    strip.background = element_rect(fill = "gray90", color = NA),
    legend.position = "none"
  )   


ggsave(
  plot = p,
  filename = "empirical-work/bikes-autoencoder/plots/ales-orig-clusters.png", 
  width = 40,
  height = 20,
  dpi = "print",
  units = "cm"
)

