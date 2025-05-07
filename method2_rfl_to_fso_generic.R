library(caret)

method2_rfl_to_fso_generic <- function(train_data, test_data, rfl_model_generic, param_grid, random_state = 42) {
  set.seed(random_state)
  
  # Step 1: Predict RFL to generate synthetic feature
  rfl_pred_train <- predict(rfl_model_generic, train_data)
  rfl_pred_test <- predict(rfl_model_generic, test_data)
  
  train_data$RFL_pred <- rfl_pred_train
  test_data$RFL_pred <- rfl_pred_test
  
  # Step 2: Run backward feature elimination
  df_for_selection <- train_data %>%
    select(-FSO_Att, -RFL_Att) %>%
    mutate(FSO_Att = train_data$FSO_Att)
  
  selection_result <- perform_feature_selection_generic(df_for_selection, target = "FSO_Att")
  threshold_idx <- selection_result$threshold_index
  log_table <- selection_result$feature_removal_log
  
  # Step 3: Determine features to keep (after threshold_idx)
  all_feats <- setdiff(names(df_for_selection), "FSO_Att")
  feats_to_remove <- log_table$Removed_Feature[1:(threshold_idx + 1)]
  final_features <- setdiff(all_feats, feats_to_remove)
  
  # Step 4: Final model training
  rf_fso_final <- train(
    x = train_data[, final_features],
    y = train_data$FSO_Att,
    method = "rf",
    trControl = trainControl(method = "cv", number = 3),
    tuneGrid = param_grid
  )
  
  # Step 5: Final evaluation: correlation between predicted FSO and RFL_pred on test set
  fso_pred <- predict(rf_fso_final, test_data[, final_features])
  correlation <- cor(fso_pred, rfl_pred_test, method = "pearson")
  
  return(list(
    correlation = correlation,
    model = rf_fso_final,
    features = final_features,
    selection_table = log_table,
    threshold_index = threshold_idx
  ))
}

