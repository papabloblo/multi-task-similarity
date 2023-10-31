#'
#'    COMPUTE WEIGHTED FRECHET DISTANCE BETWEEN ALE CURVES
#'    



# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)

source("src/frechet/frechet.R")

# LOAD ALE CURVES ---------------------------------------------------------

ale_curves <- read_rds("data/ale_by_task_var.RDS")


# FRECHET DISTANCE --------------------------------------------------------

euclid_dist <- function(x1, x2) sqrt(sum((x1-x2)**2))


fweight <- function(n1, n2) min(n1, n2)/max(n1, n2)


frechet_results <- list()

tasks <- unique(ale_curves$task)

i <- 0
for (task1 in tasks){
  for (task2 in tasks[tasks != task1]){ 
    t1 <- ale_curves[ale_curves$task == task1, ]
    t2 <- ale_curves[ale_curves$task == task2, ]
    
    for (feature1 in unique(t1$feature)){
      for (feature2 in unique(t2$feature)){
        t1_f1 <- t1[t1$feature == feature1, ]
        t2_f2 <- t2[t2$feature == feature2, ]
        
        res <- frechet(
          Px = t1_f1$x, Py = t1_f1$ale,
          Qx = t2_f2$x, Qy = t2_f2$ale,
          n1 = t1_f1$n, n2 = t2_f2$n, 
          fdist = euclid_dist,
          fweight = fweight
        )
        i <- i + 1
        frechet_results[[i]] <- list(task1 = task1, 
                                     task2 = task2,
                                     f1 = feature1,
                                     f2 = feature2,
                                     frechet = res
                                     )
      }
    }
  }
}

frechet_results[[1]]

frechet_results <- map(frechet_results, 
                       function(x) data.frame(task1 = x$task1,
                                              task2 = x$task2,
                                              f1 = x$f1,
                                              f2 = x$f2,
                                              dist_frechet = x$frechet$Dist_frechet
                                              )
                       ) %>% 
  list_rbind() %>% 
  as_tibble()


frechet_results %>% 
  filter(task1 %in% 1:2,
         task2 %in% 1:2,
         f1 %in% c("x1", "x2"),
         f2 %in% c("x1", "x2"))

frechet_results %>% 
  group_by(task1, task2, f1) %>% 
  filter(dist_frechet == min(dist_frechet)) %>% 
  group_by(task1, task2) %>% 
  summarise(sum_frechet = sum(dist_frechet)) %>% 
  arrange(task1, sum_frechet)


ale_by_task_var %>% 
  ggplot(aes(y = ale, x = x, group = task)) +
  geom_line() +
  facet_wrap(. ~task + feature, scales = "free_x") +
  labs(y = "f ale", 
       x = "", 
       title = "ALE plots by variable",
       subtitle = "Each line represents a task"
  )

