#' 
#' GENERACIÓN DE DATOS PARA LAS TAREAS
#' 
#' 

# DEPENDENCIAS ------------------------------------------------------------
library(MASS)
library(tidyverse)

source("src/aux/aux_data_generation.R")

# GENERACIÓN DE DATOS -----------------------------------------------------

# Semilla para la aleatoriedad
set.seed(2023)

# Número de tareas
n_tasks <- 5

# Número de observaciones por tarea
n <- 10000


data_generation <- function(id_task, n, fun_x4_x5){
  
  # Variables dependientes
  df <- mvrnorm(n, 
          mu = c(0, 0),
          Sigma = matrix(c(2, 1, .05, .6), 2, 2)
          ) %>% 
    data.frame()
  
  names(df) <- c("x1", "x2")
  
  df$x3 <- runif(n)
  df$x4 <- rnorm(n)
  
  df$x5 <- .2*df$x4**2 + rnorm(n, sd = .1)
  
  # Variable objetivo
  df$y <- std(rastrigin(df$x1, df$x2)) + std(fun_x4_x5(df$x4, df$x5))
  
  # Identificador de la tarea
  df$id_task <- id_task
  
  return(df)
}

# Lista de funciones que aplica cada tarea sobre las varibles x4 y x5
# TODO: leer las funciones a partir de un archivo externo de 
#       configuración de las tareas
funs_x4_x5 <- list(
  function(x4, x5) x4**2 + x5**2 + x4*x5,
  
  function(x4, x5) -x4**2 - x5**2 - x4*x5,
  
  function(x4, x5) x4**2 + x5**2 - x4*x5,
  
  function(x4, x5) x4**2 - x5**2 - x4*x5,
  
  function(x4, x5) -x4**2 - x5**2 + x4*x5
  )


# Creación del data.frame con los datos
tasks_data <- purrr::map2_df(1:n_tasks, 
               funs_x4_x5, 
               data_generation, 
               n = n)


# GUARDADO DE DATOS -------------------------------------------------------

write.csv(tasks_data, file = "data/tasks_data.csv", row.names = FALSE)


