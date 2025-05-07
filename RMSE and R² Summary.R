# Step 1: Construct Summary Table
summary_metrics <- data.frame(
  Method = character(),
  SYNOPCode = character(),
  RMSE = numeric(),
  R2 = numeric(),
  stringsAsFactors = FALSE
)

for (code in names(synop_codes)) {
  subset_test <- test_data[test_data$SYNOPCode == as.numeric(code), ]
  if (nrow(subset_test) < 30) next
  
  # Method 1
  pred_fso1 <- predict(rf_fso, subset_test)
  rmse1 <- rmse(subset_test$FSO_Att, pred_fso1)
  r2_1 <- R2(pred_fso1, subset_test$FSO_Att)
  
  # Method 2
  pred_fso2 <- predict(fso_model_method2, subset_test[, selected_feats_method2])
  rmse2 <- rmse(subset_test$FSO_Att, pred_fso2)
  r2_2 <- R2(pred_fso2, subset_test$FSO_Att)
  
  # Append both
  summary_metrics <- rbind(
    summary_metrics,
    data.frame(Method = "method1_Generic", SYNOPCode = code, RMSE = rmse1, R2 = r2_1),
    data.frame(Method = "method2_Generic", SYNOPCode = code, RMSE = rmse2, R2 = r2_2)
  )
}

# Step 2: Plot RMSE and R²
library(ggplot2)

ggplot(summary_metrics, aes(x = SYNOPCode, y = RMSE, color = Method, group = Method)) +
  geom_point(size = 3) +
  geom_line() +
  labs(title = "RMSE by Method", y = "RMSE") +
  theme_minimal()

ggplot(summary_metrics, aes(x = SYNOPCode, y = R2, color = Method, group = Method)) +
  geom_point(size = 3) +
  geom_line() +
  labs(title = "R² by Method", y = "R-squared") +
  theme_minimal()
