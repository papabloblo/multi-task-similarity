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
