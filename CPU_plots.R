library(ggplot2)
library(dplyr)
library(reshape2)
source("./own_implementation.R")

gen_cpu_plots <- function(start_beta,
                          X,
                          Y,
                          lambda,
                          max_iters,
                          how_many_rows_list) {
      
    cpu_time_n <- function(x, y, n_vector, start_beta=rep(0, 8), lambda = 0.00001, num_iters = 1000){
      cpu_time_tmp <- c()
      cpu_time_lars_tmp <- c()
      cpu_time_own <- c()
      cpu_time_lars <- c()
      for(i in 1:length(n_vector)){
        for(j in 1:5){
          rows <- sample(nrow(x), n_vector[i])
          
          cpu_time_tmp[j] <- system.time(
            coordinate_descent_lasso(
              beta = start_beta,
              X = x[rows, ],
              y = y[rows],
              lambda = lambda,
              num_iters = num_iters)
            , gcFirst = TRUE)[3]
          
          cpu_time_lars_tmp[j] <- system.time(
            coef(
              lars(
                x = x[rows, ],
                y = y[rows],
                type = "lasso",
                trace = FALSE,
                normalize = TRUE,
                intercept = FALSE,
                max.steps = num_iters
              )
            )
            , gcFirst = TRUE)[3]
        }
        cpu_time_own[i] <- mean(cpu_time_tmp)
        cpu_time_lars[i] <- mean(cpu_time_lars_tmp)
      }
      
      data.frame(x = n_vector,
                 y_own = cpu_time_own,
                 y_lars = cpu_time_lars)
    }
    
    cpu_time_p <- function(x, y, lambda = 0.00001, num_iters = 1000){
      cpu_time_own <- c()
      cpu_time_lars <- c()
      
      n_variables <- ncol(x)
      for(i in 2:n_variables){
          
        cpu_time_own[i] <- system.time(
            coordinate_descent_lasso(
              beta = rep(0, i),
              X = x[,1:i],
              y = y,
              lambda = lambda,
              num_iters = num_iters)
            , gcFirst = TRUE)[3]
          
        cpu_time_lars[i] <- system.time(
            coef(
              lars(
                x = x[,1:i],
                y = y,
                type = "lasso",
                trace = FALSE,
                normalize = TRUE,
                intercept = FALSE,
                max.steps = num_iters
              )
            )
            , gcFirst = TRUE)[3]
      }
      
      data.frame(x = 2:n_variables,
                 y_own = cpu_time_own[2:n_variables],
                 y_lars = cpu_time_lars[2:n_variables])
    }
    
    
    prepare_line_plot <- function(df, xlab, ylab) {
      iter_cost_plot <- ggplot(df, aes(x = x, y = y_own)) +
        geom_line(aes(color = "own")) +
        geom_line(aes(x = x, y = y_lars, color = "lars")) +
        xlab(xlab) +
        ylab(ylab) +
        labs(color = "method") +
        theme_classic() +
        theme(
          axis.text.x = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14),
          axis.title.y = element_text(size = 16),
          plot.title = element_text(
            size = 20,
            face = "bold",
            color = "darkgreen"
          )
        )
      iter_cost_plot
    }
    
    df_n <- cpu_time_n(x,y,n_vector = how_many_rows_list, start_beta = start_beta)
    df_p <- cpu_time_p(x,y)
    
    xlab <- "number of samples"
    ylab <- "CPU time [seconds]"
    cpu_n_plot <- prepare_line_plot(df_n, xlab, ylab)
    
    xlab <- "number of variabes"
    ylab <- "CPU time [seconds]"
    cpu_p_plot <- prepare_line_plot(df_p, xlab, ylab)
    
    return(
      list(
        cpu_n_plot=cpu_n_plot,
        cpu_p_plot=cpu_p_plot
      )
    )
}