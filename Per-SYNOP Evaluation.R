synop_codes <- c("0" = "clear", "3" = "dust storm", "4" = "fog", "5" = "drizzle",
                 "6" = "rain", "7" = "snow", "8" = "showers")

results <- list()

for (code in names(synop_codes)) {
  subset_test <- test_data[test_data$SYNOPCode == as.numeric(code), ]
  
  if (nrow(subset_test) < 30) next
  
  # Example: compare measured FSO_Att vs predicted RFL_Att (Method 2)
  rfl_pred <- predict(rf_rfl, subset_test)
  fso_pred_method2 <- predict(fso_model_method2, subset_test[, selected_feats_method2])
  
  # Pearson correlation
  corr <- cor(fso_pred_method2, rfl_pred, method = "pearson")
  
  # Joint entropy + heatmap
  joint_result <- joint_entropy_and_heatmap(fso_pred_method2, rfl_pred, bin_width = 0.5)
  
  results[[code]] <- list(
    pearson = corr,
    joint_entropy = joint_result$joint_entropy,
    heatmap = joint_result$plot
  )
  
  print(paste("SYNOP", code, "- Pearson:", round(corr, 3), "Joint H:", round(joint_result$joint_entropy, 3)))
}
