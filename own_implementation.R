stringsAsFactors = FALSE
library(dplyr)
library(lars)

setwd("C:/Users/tomas/OneDrive/Documents/Studies/PW-IAD/Optymalizacja/")
diabetes <- read.csv2("diabetes.csv", sep = ',', header = TRUE, stringsAsFactors = FALSE)
diabetes <- transform(diabetes, BMI = as.numeric(BMI))
diabetes <- transform(diabetes, DiabetesPedigreeFunction = as.numeric(DiabetesPedigreeFunction))

x <- as.matrix(diabetes %>% select(-c("Outcome")))
y <- as.vector(diabetes %>% select(c("Outcome")))
y <- unlist(y)
x[is.na(x)] <- 0
x <- x / norm(x, type="2") # Normalize data

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
  for(step in 1:(num_iters + 1)){
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
  #  print(cost_iter[step + 1])
    
    diff[step] <- sum((b - prev_b)**2)
    
    if(diff[step] < eps) {
      print(step + 1)
      break
    }
  }
  
#  cost_iter <- cost_iter[!is.na(cost_iter)]
  list(beta=b, cost_val=cost_iter, diff=diff)
}

compare <- function() {
  own_res <- coordinate_descent_lasso(beta=rep(0,8),
                                X=x,
                                Y=y,
                                lambda=0.0000001,
                                num_iters = 1000)
  
  lars_coef <- coef(lars(x,y, type = "lasso", trace = FALSE, normalize = TRUE, intercept = FALSE))
  print(own_res$beta)
  print(as.numeric(lars_coef[nrow(lars_coef),]))
}

compare()

