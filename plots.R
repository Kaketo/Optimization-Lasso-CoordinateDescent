library(ggplot2)
library(dplyr)
library(reshape2)
source("./own_implementation.R")

gen_plots <- function(start_beta,
           X,
           Y,
           lambda,
           max_iters,
           lambda_list) {
    # 1: iteracja vs wartość funkcji kosztu
    res_own <- coordinate_descent_lasso(
      beta = start_beta,
      X = X,
      Y = Y,
      lambda = lambda,
      num_iters = max_iters
    )
    
    get_lars_coef <- function(iter_num) {
      coef_lars <-
        coef(
          lars(
            X,
            Y,
            type = "lasso",
            trace = FALSE,
            normalize = TRUE,
            intercept = FALSE,
            max.steps = iter_num
          )
        )
      coef_lars <- as.numeric(coef_lars[nrow(coef_lars), ])
      coef_lars
    }
    
    cost_fun_lars <- sapply(1:(max_iters), function(iter_num) {
      coef_lars <- get_lars_coef(iter_num)
      cost_fun(x, y, coef_lars, lambda)
    })
    
    df <- data.frame(
      x = 0:(length(res_own$cost_val) - 1),
      y_own = res_own$cost_val,
      y_lars = c(res_own$cost_val[1], cost_fun_lars)
    )
    
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
    
    
    xlab <- "iteration number"
    ylab <- "cost function value"
    iter_cost_plot_1 <-
      prepare_line_plot(df[1:4, ], xlab = xlab, ylab = ylab)
    
    iter_cost_plot_2 <-
      prepare_line_plot(df[3:50, ], xlab = xlab, ylab = ylab)
    
    iter_cost_plot_3 <-
      prepare_line_plot(df[10:50, ], xlab = xlab, ylab = ylab)
    
    iter_cost_plot_4 <-
      prepare_line_plot(df[50:100, ], xlab = xlab, ylab = ylab)
    
    iter_cost_plot_5 <-
      prepare_line_plot(df[100:150, ], xlab = xlab, ylab = ylab)
    
    iter_cost_plot_m <-
      prepare_line_plot(df[1:300, ], xlab = xlab, ylab = ylab)
    
    # 2: rozne lambdy vs kazdy wspolczynnik osobno
    
    res_own_betas <- lapply(lambda_list, function(lambda) {
      coordinate_descent_lasso(
        beta = start_beta,
        X = X,
        Y = Y,
        lambda = lambda,
        num_iters = max_iters
      )$beta
    })
    
    df_2_own <-
      cbind(as.data.frame(matrix(
        unlist(res_own_betas),
        ncol = 8,
        nrow = length(lambda_list)
      )), data.frame(lambda = log10(lambda_list)))
    df_melted_own <- melt(df_2_own, id.vars = "lambda")
    beta_plot_own <-
      ggplot(data = df_melted_own, aes(x = lambda, y = value, color = variable)) +
      geom_line() +
      xlab("log10(lambda)") +
      theme_classic()
    
    # 3: iteracja vs predkosc zmian wspolczynnikow (druga norma do kwadratu)
    
    lars_coef <- lapply(1:max_iters, function(iter_num) {
      get_lars_coef(iter_num)
    })
    
    lars_diff <- sapply(2:max_iters, function(iter_num) {
      sum(sqrt((lars_coef[[iter_num]] - lars_coef[[iter_num - 1]]) ** 2))
    })
    
    lars_diff <- c(sqrt(sum(lars_coef[[1]]**2)), lars_diff)
    
    df_3 <- data.frame(x = 1:max_iters,
                       y_own = res_own$diff,
                       y_lars = lars_diff)
    
    xlab_3 <- "iteration number"
    ylab_3 <- "norm_2(beta diff)"
    beta_change_plot_1 <-
      prepare_line_plot(df_3[1:100,], xlab = xlab_3, ylab = ylab_3)
    
    beta_change_plot_2 <-
      prepare_line_plot(df_3[1:5,], xlab = xlab_3, ylab = ylab_3)
    
    beta_change_plot_3 <-
      prepare_line_plot(df_3[5:10,], xlab = xlab_3, ylab = ylab_3)
    
    beta_change_plot_4 <-
      prepare_line_plot(df_3[10:30,], xlab = xlab_3, ylab = ylab_3)
    
    
    return (
      list(
        iter_cost_plot_1 = iter_cost_plot_1,
        iter_cost_plot_2 = iter_cost_plot_2,
        iter_cost_plot_3 = iter_cost_plot_3,
        iter_cost_plot_4 = iter_cost_plot_4,
        iter_cost_plot_5 = iter_cost_plot_5,
        iter_cost_plot_m = iter_cost_plot_m,
        beta_plot_own = beta_plot_own,
        beta_change_plot_1 = beta_change_plot_1,
        beta_change_plot_2 = beta_change_plot_2,
        beta_change_plot_3 = beta_change_plot_3,
        beta_change_plot_4 = beta_change_plot_4
      )
    )
  }
