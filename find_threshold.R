find_threshold <- function(data_table, r2_threshold = 0.01, rmse_threshold = 0.02) {
  for (i in 1:(nrow(data_table) - 1)) {
    r2_decrease <- (data_table$R2[i] - data_table$R2[i + 1]) / data_table$R2[i]
    rmse_increase <- (data_table$RMSE[i + 1] - data_table$RMSE[i]) / data_table$RMSE[i]
    if (r2_decrease > r2_threshold && rmse_increase > rmse_threshold) {
      return(i)
    }
  }
  return(nrow(data_table))  # fallback if threshold not met
}