stringsAsFactors = FALSE
library(dplyr)
library(lars)

soft_thresholding_operator <- function(x, lambda){
  if (x>0 && lambda<abs(x)){
    return(x-lambda)
  }else if (x<0 && lambda<abs(x)){
    return(x+lambda)
  }else{
    return(0)
  }
}

cost_fun <- function(X, y, beta, lambda) {
  1/2 * sum((y - X %*% beta)**2) + lambda * sum(abs(beta))
}

coordinate_descent_lasso <- function(beta, X, Y, lambda=0.01, num_iters=100, eps=1e-25) {
  # Initial values
  p <- ncol(X)
  b <- beta # Initial betas
  
  cost_iter = rep(NA, num_iters + 1)
  cost_iter[1] = cost_fun(X, y, beta, lambda)
  
  diff <- rep(NA, num_iters - 1)
  
  # Coordiante descent
  for(step in 1:(num_iters)){
    prev_b <- b
    
    for(j in 1:p){
      tmp_b <- b
      tmp_b[j] <- 0
      r_j <- y - X %*% tmp_b
      
      rho_j <- X[,j] %*% r_j
      
      normalizing_parameter <- sum((X[,j])**2)
        
      b[j] <- soft_thresholding_operator(rho_j, lambda) / normalizing_parameter
    }
    
    cost_iter[step + 1] <- cost_fun(X, Y, b, lambda)

    diff[step] <- sqrt(sum((b - prev_b)**2))
    
    if(diff[step] < eps) {
      print(step + 1)
      break
    }
  }
  
  list(beta=b, cost_val=cost_iter, diff=diff)
}
