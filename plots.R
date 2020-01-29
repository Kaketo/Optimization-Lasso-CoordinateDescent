library(ggplot2)
library(dplyr)
source("./own_implementation.R")

# 1: iteracja vs wartość funkcji kosztu
max_iters <- 1000
res_own <- coordinate_descent_lasso(beta=rep(0,8),
                                X=x,
                                Y=y,
                                lambda=0.0,
                                num_iters = max_iters)

cost_fun_lars <- sapply(0:(max_iters), function(iter_num) {
  coef_lars <- coef(lars(x,y, type = "lasso", trace = FALSE, normalize = TRUE, intercept = FALSE, max.steps = iter_num))
  coef_lars <- as.numeric(coef_lars[nrow(coef_lars),])
  cost_fun(x, y, coef_lars, lambda)
})

df <- data.frame(
  x=0:(length(res_own$cost_val)-1),
  y_own=res_own$cost_val,
  y_lars=cost_fun_lars
)

df <- df[1:50,]

iter_cost_plot <- ggplot(df, aes(x=x, y=y_own)) +
  geom_line(aes(color="own")) +
  geom_line(aes(x=x, y=y_lars, color="lars")) +
  xlab("iteration number") +
  ylab("cost function value") +
  labs(color="method") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"))

iter_cost_plot
