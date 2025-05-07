# Run Method 2
method2_result <- method2_rfl_to_fso_generic(
  train_data = train_data,
  test_data = test_data,
  rfl_model_generic = rf_rfl,
  param_grid = expand.grid(mtry = c(2, 4, 6))  # example grid
)

# Extract results
fso_model_method2 <- method2_result$model
selected_feats_method2 <- method2_result$features
fso_table_method2 <- method2_result$selection_table
fso_threshold <- method2_result$threshold_index

# Plot performance of feature elimination
plot_feature_selection(
  rfl_table = rfl_table_final,
  fso_table = fso_table_method2,
  rfl_threshold = NA,  # or use value if comparing
  fso_threshold = fso_threshold,
  title = "Feature Selection Comparison (Method 2)"
)
