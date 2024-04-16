
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
  df$y <- y
  
  df$id_task <- task_config[["id_task"]]
  return(df)
}
