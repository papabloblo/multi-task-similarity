frechet <- function(Px, Py, 
                    Qx, Qy, 
                    n1, n2, 
                    fdist, 
                    fweight, 
                    FrechetSumOrMax = "sum"){
  
  P <- matrix(NA, nrow = length(Px), ncol = 2)
  P[,1] <- Px
  P[,2] <- Py
  
  Q <- matrix(NA, nrow = length(Qx), ncol = 2)
  Q[,1] <- Qx
  Q[,2] <- Qy
  
  maxP <- length(Px)
  maxQ <- length(Qx)
  Mdist <- Mfret <- matrix(data = 0, 
                           nrow = maxP, 
                           ncol = maxQ, 
                           dimnames = list(paste0("P", 1:maxP), 
                                           paste0("Q", 1:maxQ)
                                           )
                           )
  
  # if (isFALSE(weight)) weight <- rep(1, maxP)
  
  for (i in 1:maxP) {
    for (j in 1:maxQ) {
      Mdist[i, j] <- fdist(P[i,], Q[j,]) * fweight(n1[i], n2[j])
      
      if (i == 1 && j == 1) {
        Mfret[1, 1] = Mdist[1, 1]
      }
      if (i > 1 && j == 1) {
        Mfret[i, 1] = do.call(FrechetSumOrMax, 
                              list(Mfret[i-1, 1], Mdist[i, 1])
                              )
      }
      if (i == 1 && j > 1) {
        Mfret[1, j] = do.call(FrechetSumOrMax, 
                              list(Mfret[1, j-1], Mdist[1, j])
                              )
      }
      if (i > 1 && j > 1) {
        Mfret[i, j] = do.call(FrechetSumOrMax, 
                              list(min(Mfret[i-1, j  ],
                                       Mfret[i-1, j-1], 
                                       Mfret[i  , j-1]
                                       ),
                                   Mdist[i, j]
                                   )
                              )
        }
    }
  }
  return(
    list(Dist_frechet = Mfret[maxP, maxQ],
         Mfret = Mfret, Mdist = Mdist
         )
    )
}


frechet_taks_feature <- function(ale_curves, fdist, fweight) {
  
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
            fdist = fdist,
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
  return(frechet_results)
}


frechet_summary <- function(frechet_results){
  return(
    map(frechet_results, 
        function(x) data.frame(task1 = x$task1,
                               task2 = x$task2,
                               f1 = x$f1,
                               f2 = x$f2,
                               dist_frechet = x$frechet$Dist_frechet
        )
    ) %>% 
      list_rbind() %>% 
      as_tibble()
  )
}


importance <- function(df, model = randomForest, predictors = "all", response = "y"){
  
  list_importance <- list()
  for (task in unique(df$id_task)){
    df_task <- df[df$id_task == task, ]
    df_task$id_task <- NULL
    
    if (predictors[1] == "all") 
      predictors <- names(df_task)[names(df_task) != response]
    
    mod <- model(
      reformulate(response = response, termlabels = predictors),
      data = df_task,
      importance = TRUE
    )
    
    imp <- mod$importance[, "IncNodePurity"]
    
    list_importance[[task]] <- imp/sum(imp)
  }
  return(list_importance)
}
