#'
#' OBJETIVO:
#' entender el algoritmo de cálculo de la distancia de Frechet
#' que está programado en la función distFrechet del
#' paquete longitudinalData.
#' 
#' El algoritmo está en el paper: 
#' http://www.kr.tuwien.ac.at/staff/eiter/et-archive/cdtr9464.pdf
#'    


# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)
library(longitudinalData)

# DATOS DE TESTEO ---------------------------------------------------------

ale_by_var <- readRDS("data/ale_by_var.RDS")

task1 <- 1
task2 <- 2

var_name <- "x1"


Px <- ale_by_var$x[ale_by_var$feature == var_name & ale_by_var$task == task1]
Py <- ale_by_var$y[ale_by_var$feature == var_name & ale_by_var$task == task1]

Qx <- ale_by_var$x[ale_by_var$feature == var_name & ale_by_var$task == task2]
Qy <- ale_by_var$y[ale_by_var$feature == var_name & ale_by_var$task == task2]

P <- matrix(NA, nrow = length(Px), ncol = 2)
P[,1] <- Px
P[,2] <- Py

Q <- matrix(NA, nrow = length(Qx), ncol = 2)
Q[,1] <- Qx
Q[,2] <- Qy

tibble(x1 = Px, y1 = Py, x2 = Qx, y2 = Qy) %>% 
ggplot() +
  geom_line(aes(x = x1, y = y1)) +
  geom_line(aes(x = x2, y = y2)) 


timeScale = .1 #¿Qué hace este parámetro?
FrechetSumOrMax = "sum"



maxP <- length(Px)
maxQ <- length(Qx)
Mdist <- Mfret <- matrix(data = 0, 
                         nrow = maxP, 
                         ncol = maxQ, 
                         dimnames = list(paste0("P", 1:maxP), 
                                         paste0("Q", 1:maxQ)
                                         )
                         )
                   
euc <- function(x1, x2) sqrt(sum((x1-x2)**2))
for (i in 1:maxP) {
  for (j in 1:maxQ) {
    Mdist[i, j] <- euc(P[i,], Q[j,])
                        
    
    if (i == 1 && j == 1) {
      Mfret[1, 1] = Mdist[1, 1]
    }
    if (i > 1 && j == 1) {
      Mfret[i, 1] = do.call(FrechetSumOrMax, 
                            list(Mfret[i - 1, 1], Mdist[i, 1]))
    }
    if (i == 1 && j > 1) {
      Mfret[1, j] = do.call(FrechetSumOrMax, 
                            list(Mfret[1,j - 1], Mdist[1, j]))
    }
    if (i > 1 && j > 1) {
      Mfret[i, j] = do.call(FrechetSumOrMax, 
                            list(min(Mfret[i - 1, j    ],
                                     Mfret[i - 1, j - 1], 
                                     Mfret[i    , j - 1]
                                     ),
                                 Mdist[i, j]
                                 )
                            )
    }
  }
}
print(Mdist)
print(Mfret)
cat("\n\n Result =", Mfret[maxP, maxQ], "\n")
return(Mfret[maxP, maxQ])
