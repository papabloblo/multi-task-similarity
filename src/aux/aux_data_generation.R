
rastrigin <- function(x1, x2){
  20 + x1**2 - 10*cos(2*pi*x1) + x2**2 - 10*cos(2*pi*x2)
}

std <- function(x) (x - mean(x))/sd(x)