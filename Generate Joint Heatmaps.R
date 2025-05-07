methods <- c("measured", "method1_Generic", "method2_Generic", "method3_Generic")

# Initialize list to store plots
all_heatmaps <- list()

for (method in methods) {
  for (code in names(synop_codes)) {
    subset_test <- test_data[test_data$SYNOPCode == as.numeric(code), ]
    
    if (nrow(subset_test) < 30) next
    
    if (method == "measured") {
      X <- subset_test$FSO_Att
      Y <- subset_test$RFL_Att
    } else if (method == "method1_Generic") {
      X <- predict(rf_fso, subset_test)
      Y <- predict(rf_rfl, subset_test)
    } else if (method == "method2_Generic") {
      X <- predict(fso_model_method2, subset_test[, selected_feats_method2])
      Y <- predict(rf_rfl, subset_test)
    } else if (method == "method3_Generic") {
      X <- predict(rf_rfl, subset_test)
      Y <- predict(rf_fso, subset_test)
    }
    
    # Generate joint heatmap
    result <- joint_entropy_and_heatmap(X, Y, bin_width = 0.5, title = paste(method, "-", synop_codes[[code]]))
    all_heatmaps[[paste(method, code)]] <- result$plot
  }
}
