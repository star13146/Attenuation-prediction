# Step 1: Organize Data
summary_table <- data.frame(
  Method = character(),
  SYNOPCode = character(),
  Correlation = numeric(),
  NMI = numeric(),
  stringsAsFactors = FALSE
)

for (code in names(synop_codes)) {
  summary_table <- rbind(summary_table, data.frame(
    Method = "method2_Generic",
    SYNOPCode = code,
    Correlation = results[[code]]$pearson,
    NMI = 1 - results[[code]]$joint_entropy / log2(length(results[[code]]$joint_entropy)),  # pseudo-NMI
    stringsAsFactors = FALSE
  ))
}

#  Step 2: Plot Summary
library(ggplot2)

ggplot(summary_table, aes(x = SYNOPCode, y = Correlation, color = Method)) +
  geom_point(size = 3) +
  geom_line(aes(group = Method)) +
  labs(title = "Pearson Correlation by Method", y = "Correlation") +
  theme_minimal()

ggplot(summary_table, aes(x = SYNOPCode, y = NMI, color = Method)) +
  geom_point(size = 3) +
  geom_line(aes(group = Method)) +
  labs(title = "Normalized Mutual Information by Method", y = "NMI") +
  theme_minimal()
