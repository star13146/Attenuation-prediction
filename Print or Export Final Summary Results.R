
cat("Normalized Mutual Information for Method 2 (RFL → FSO):\n")
for (code in names(results)) {
  nmi <- 1 - results[[code]]$joint_entropy / log2(nrow(test_data))
  cat(sprintf("SYNOP %s (%s): NMI = %.4f\n", code, synop_codes[[code]], nmi))
}

write.csv(summary_metrics, "method_comparison_summary.csv", row.names = FALSE)

pdf("rmse_r2_summary_plots.pdf")
print(ggplot(summary_metrics, aes(x = SYNOPCode, y = RMSE, color = Method)) +
        geom_point() + geom_line() + ggtitle("RMSE by Method"))
print(ggplot(summary_metrics, aes(x = SYNOPCode, y = R2, color = Method)) +
        geom_point() + geom_line() + ggtitle("R² by Method"))
dev.off()
