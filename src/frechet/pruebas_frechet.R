#' 
#' OBJETIVO
#' 
#' Simular unas cuantas curvas con la distribución de puntos.
#' 
#' Entender qué ponderación sería adecuada para la
#' distancia entre curvas ALE.
#' 


# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)
source("src/frechet/frechet.R")


# SIMULACIÓN DE LAS CURVAS ------------------------------------------------

n <- 100

curves <- tibble(
  x = seq(from = 0, to = 10, length.out = n),
  
  y1 = (x - 1.5)*(x - 5)*(x - 9),
  y2 = (x - 1.5)*(x - 5)*(x - 9) + 10,
  y3 = (x - 1.5)*(x - 5)*(x - 7),
  
  w0 = rep(1, length.out = n)/n,
  w1 = (n:1)/n,
  w2 = exp((n:1)/50),
  w3 = c(rep(1, 50), rep(0.1, 50))
)


curves <- curves %>% 
  pivot_longer(starts_with("y"), names_to = "task", values_to = "y") %>% 
  pivot_longer(starts_with("w"), names_to = "type_weight", values_to = "weight")



curves %>% 
  ggplot(
    aes(x = x, y = y, group = task, color = task, linewidth = weight)
  ) +
  geom_line() + 
  facet_wrap(vars(type_weight))



# DISTANCIA DE FRECHET ----------------------------------------------------

# Función de distancia

fdist <- function(x1, x2) sqrt(sum((x1-x2)**2))

#' Frechet distance between tasks considering the weight
#'
#' @param curves 
#' @param fdist 
#'
#' @return
#' @export
#'
#' @examples
frechet_tasks_weights <- function(curves, fdist) {
  
  frechet_results <- list()
  tasks <- unique(curves$task)
  
  i <- 0
  for (task1 in 1:(length(tasks)-1)){
    for (task2 in (task1 + 1):length(tasks)){
      for (w in unique(curves$type_weight)){
        i <- i + 1
        t1 <- curves[curves$task == tasks[task1] & curves$type_weight == w, ]
        t2 <- curves[curves$task == tasks[task2] & curves$type_weight == w, ]
        
        res <- frechet(
          Px = t1$x,
          Py = t1$y,
          Qx = t2$x,
          Qy = t2$y,
          fdist = fdist,
          weight = t1$weight
        )
        
        frechet_results[[i]] <- list(task1 = tasks[task1], 
                                  task2 = tasks[task2],
                                  weight = w,
                                  frechet = res
                                  )
                             
      }  
    }
  }
  
  frechet_results <- map(frechet_results, 
      function(x) data.frame(weight = x$weight,
                             task1 = x$task1,
                             task2 = x$task2,
                             dist_frechet = x$frechet$Dist_frechet
      )
  ) %>% 
    list_rbind() %>% 
    arrange(weight, task1, task2)
  
  return(frechet_results)
}

frechet_tasks_weights(curves, fdist)

# PRUEBAS CON LAS CURVAS ALE -----------------------------------------------

ale_by_var <- readRDS("data/ale_by_var.RDS")

x1 <- ale_by_var %>% 
  filter(task == 1, feature == "x1")

list_ales[[1]]$x1

df_t1 <- df[df$id_task == 1, ]
df_t1 <- tibble(df_t1)

quantile(df_t1$x1, probs = (0:30)/30)
x1$x
