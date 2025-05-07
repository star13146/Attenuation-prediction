# install.packages("entropy")
library(entropy)
library(ggplot2)
compute_entropy <- function(p_vec) {
  p_vec <- p_vec[p_vec > 0]  # remove zero-prob bins
  -sum(p_vec * log2(p_vec))
}


joint_entropy_and_heatmap <- function(X, Y, bin_width = 0.5, title = "Joint Distribution") {
  df <- data.frame(X = X, Y = Y)
  
  # Bin data
  df$X_bin <- cut(df$X, breaks = seq(min(df$X), max(df$X), by = bin_width), include.lowest = TRUE)
  df$Y_bin <- cut(df$Y, breaks = seq(min(df$Y), max(df$Y), by = bin_width), include.lowest = TRUE)
  
  # Count joint occurrences
  joint_table <- table(df$X_bin, df$Y_bin)
  joint_probs <- joint_table / sum(joint_table)
  joint_entropy <- compute_entropy(as.vector(joint_probs))
  
  # Heatmap plot
  heatmap_df <- as.data.frame(joint_table)
  colnames(heatmap_df) <- c("X_bin", "Y_bin", "Freq")
  
  plot <- ggplot(heatmap_df, aes(x = X_bin, y = Y_bin, fill = Freq)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(title = title, x = "X", y = "Y") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))
  
  list(joint_entropy = joint_entropy, plot = plot)
}

