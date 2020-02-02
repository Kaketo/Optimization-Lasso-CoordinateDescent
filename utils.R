library(lars)

get_lars_coef <- function(X, Y, iter_num) {
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

prepare_line_plot <- function(df, xlab, ylab) {
  iter_cost_plot <- ggplot(df, aes(x = x, y = y_own)) +
    geom_line(aes(color = "coordinate descent"))
  
  if(!is.null(df$y_lars)) {
    iter_cost_plot <- iter_cost_plot + geom_line(aes(x=x, y=y_lars))
  }
  iter_cost_plot <- iter_cost_plot +
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
