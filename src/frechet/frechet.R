euclid_dist <- function(x1, x2) sqrt(sum((x1-x2)**2))

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


frechet_tasks_feature <- function(ale_curves, 
                                 fweight,
                                 importance,
                                 fdist = euclid_dist,
                                 same_features = TRUE, 
                                 std = TRUE) {
  
  if (std){
    ale_curves <- ale_curves %>% 
      group_by(feature) %>% 
      mutate(x = (x - mean(x))/sd(x))  
  }
  
  tasks <- unique(ale_curves$task)
  
  frechet_results <- list()
  i <- 0
  for (task1 in tasks){
    for (task2 in tasks[tasks != task1]){ 
      t1 <- ale_curves[ale_curves$task == task1, ]
      t2 <- ale_curves[ale_curves$task == task2, ]
      
      if (same_features){
        features <- data.frame(feature1 = unique(t1$feature),
                               feature2 = unique(t1$feature))
      } else {
        features <- expand.grid(feature1 = unique(t1$feature),
                                feature2 = unique(t2$feature))
      }
      
      for (f in 1:nrow(features)){
        
          t1_f1 <- t1[t1$feature == features$feature1[f], ]
          t2_f2 <- t2[t2$feature == features$feature2[f], ]
          
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
                                       f1 = features$feature1[f],
                                       f2 = features$feature2[f],
                                       frechet = res,
                                       imp = importance[[task1]]
                                       )
        
      }
    }
  }
  return(frechet_results)
}


frechet_task_var_summary <- function(frechet_results){
  
  
    df <- map(frechet_results, 
              function(x) data.frame(task1 = x$task1,
                                     task2 = x$task2,
                                     f1 = x$f1,
                                     f2 = x$f2,
                                     dist_frechet = x$frechet$Dist_frechet
                                     ) %>% 
                left_join(
                  data.frame(importance = x$imp) %>%
                    rownames_to_column(var = "f1"),
                  by = "f1"
                  )
              ) %>% 
      list_rbind() %>% 
      as_tibble()
    
    df <- df %>% 
      group_by(task1, task2, f1) %>% 
      filter(dist_frechet == min(dist_frechet)) %>% 
      ungroup() %>% 
      mutate(wdist = dist_frechet*importance)
      
  return(df)
}


frechet_task_summary <- function(frechet_results){
  frechet_task_var_summary(frechet_results) %>% 
    group_by(task1, task2) %>% 
    summarise(similarity = sum(wdist)) %>% 
    ungroup() %>% 
    arrange(task1, similarity) %>% 
    return()
}
