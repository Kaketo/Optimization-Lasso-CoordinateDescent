source("own_implementation.R")
source("utils.R")

diabetes <- read.csv2("diabetes.csv", sep = ',', header = TRUE, stringsAsFactors = FALSE)
diabetes <- transform(diabetes, BMI = as.numeric(BMI))
diabetes <- transform(diabetes, DiabetesPedigreeFunction = as.numeric(DiabetesPedigreeFunction))

x <- as.matrix(diabetes %>% select(-c("Outcome")))
y <- as.vector(diabetes %>% select(c("Outcome")))
y <- unlist(y)
x[is.na(x)] <- 0
x <- x / norm(x, type="2") # Normalize 
start_beta <- rep(0,8)

lambda_list <- c(0.00001, 0.0001, 0.001, 0.01, 0.1, 0.5, 1, 2)
max_iters <- 1000

lr <- simplify2array(get_lars_coef(x, y, max_iters))
mse_list <- sapply(lambda_list, function(lambda) {
  cd <- simplify2array(coordinate_descent_lasso(
    beta = start_beta,
    X = x,
    y = y,
    lambda = lambda,
    num_iters = max_iters
  ))$beta
  
  computed_res <- as.vector(x %*% cd)
  expected_res <- as.vector(y)

  sqrt(sum((computed_res - expected_res)**2))
})

print(mse_list)
