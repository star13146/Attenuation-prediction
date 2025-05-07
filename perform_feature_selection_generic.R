library(randomForest)
library(Metrics)
library(dplyr)

perform_feature_selection_generic <- function(df, target, param_random_state = 42) {
  set.seed(param_random_state)
  
  # Separate features and target
  remaining_features <- setdiff(colnames(df), target)
  
  results <- data.frame(Removed_Feature = character(), RMSE = numeric(), R2 = numeric(), stringsAsFactors = FALSE)
  
  while (length(remaining_features) > 1) {
    rf <- randomForest(df[, remaining_features], df[[target]], importance = TRUE)
    
    preds <- predict(rf, df[, remaining_features])
    rmse_val <- rmse(df[[target]], preds)
    r2_val <- R2(preds, df[[target]])
    
    imp <- importance(rf, type = 1)  # MeanDecreaseAccuracy or IncNodePurity
    least_imp_feature <- rownames(imp)[which.min(imp[, 1])]
    
    results <- rbind(results, data.frame(Removed_Feature = least_imp_feature, RMSE = rmse_val, R2 = r2_val))
    remaining_features <- setdiff(remaining_features, least_imp_feature)
  }
  
  return(list(feature_removal_log = results, threshold_index = find_threshold(results)))
}


