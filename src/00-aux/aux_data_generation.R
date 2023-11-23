
rastrigin <- function(x1, x2, standard = TRUE){
  y <- 20 + x1**2 - 10*cos(2*pi*x1) + x2**2 - 10*cos(2*pi*x2)
  if (standard){
    y <- (y-mean(y))/sd(y)
  }
  return(y)
}

std <- function(x) (x - mean(x))/sd(x)



data_generation <- function(task_config){
  df <- list()
  for (x in names(task_config)[substr(names(task_config), 1, 1) == "x"]){
    args <- lapply(task_config[[x]]$args, 
                   function(x){
                     if (is.character(x)) {
                       eval(parse(text = x))
                    } else {x}
                   }) 
    df[[x]] <- do.call(task_config[[x]]$f,
                     args = args
                     )
    if (grepl("_", x)) {
      var_names <- strsplit(x, "_")[[1]]
      for (x_aux in seq_along(var_names)){
        df[[var_names[x_aux]]] <- df[[x]][, x_aux]
      }
      df[[x]] <- NULL
    }
  }
  
  y <- do.call(eval(parse(text = task_config$y)), args = df)
  
  df <- as.data.frame(df)
  # df <- std(df)
  df$y <- y
  
  df$id_task <- task_config[["id_task"]]
  return(df)
}




# DEPRECATED --------------------------------------------------------------
# 
# data_generation <- function(id_task, n_obs, fun_x4_x5){
#   
#   # Features
#   x1_x2 <- mvrnorm(n_obs, 
#                    mu = c(0, 0),
#                    Sigma = matrix(c(2, 1, .05, .6), 2, 2)
#   )
#   
#   colnames(x1_x2) <- c("x1", "x2")
#   
#   x3 <- runif(n_obs)
#   
#   x4 <- rnormMix(n_obs, 
#                  mean1 = .25, sd1 = .1, 
#                  mean2 = .75, sd2 = .1
#   )
#   
#   x5 <- .2*x4**2 + rnorm(n_obs, sd = .1)
#   
#   x4 <- do.call(task1$x4$f, 
#                 args = c(task1$x4$args, n = n_obs))
#   
#   # Target
#   y <- rastrigin(x1_x2[,1], x1_x2[,2]) + fun_x4_x5(x4, x5)
#   
#   # Final data.frame
#   df <- data.frame(
#     x1 = x1_x2[, 1],
#     x2 = x1_x2[, 2],
#     x3,
#     x4,
#     x5,
#     y
#   )
#   
#   df <- std(df)
#   df$id_task <- id_task
#   
#   return(df)
# }
