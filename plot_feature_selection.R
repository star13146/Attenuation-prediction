library(ggplot2)
library(gridExtra)

plot_feature_selection <- function(rfl_table, fso_table, rfl_threshold = NULL, fso_threshold = NULL, title = "Feature Selection Performance") {
  rfl_table$Index <- seq_len(nrow(rfl_table))
  fso_table$Index <- seq_len(nrow(fso_table))
  
  plot_rfl <- ggplot(rfl_table, aes(x = Index)) +
    geom_line(aes(y = RMSE, color = "RMSE")) +
    geom_line(aes(y = R2, color = "R2")) +
    scale_color_manual(values = c("RMSE" = "blue", "R2" = "red")) +
    labs(title = "RFL Feature Selection", x = "Feature Removal Step", y = "") +
    theme_minimal()
  
  if (!is.null(rfl_threshold)) {
    plot_rfl <- plot_rfl + geom_vline(xintercept = rfl_threshold, linetype = "dashed", color = "black")
  }
  
  plot_fso <- ggplot(fso_table, aes(x = Index)) +
    geom_line(aes(y = RMSE, color = "RMSE")) +
    geom_line(aes(y = R2, color = "R2")) +
    scale_color_manual(values = c("RMSE" = "blue", "R2" = "red")) +
    labs(title = "FSO Feature Selection", x = "Feature Removal Step", y = "") +
    theme_minimal()
  
  if (!is.null(fso_threshold)) {
    plot_fso <- plot_fso + geom_vline(xintercept = fso_threshold, linetype = "dashed", color = "black")
  }
  
  grid.arrange(plot_rfl, plot_fso, ncol = 2, top = title)
}
