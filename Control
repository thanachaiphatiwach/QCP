cat("\n--- CONTROL Phase: Post-Improvement Process Capability ---\n")

# Load the improved data into 'data' for consistent monitoring
data <- read.csv("automotive_manufacturing_improved_data.csv")

# Get capability for improved Bore_Diameter_mm using 'data'
improved_bore_capability <- calculate_capability(data$Bore_Diameter_mm, USL, LSL)

cat(sprintf("Improved Process Mean (Bore Diameter): %.4f mm\n", improved_bore_capability$mean))
cat(sprintf("Improved Process Standard Deviation (Bore Diameter): %.4f mm\n", improved_bore_capability$sd))
cat(sprintf("Calculated Cp (Improved): %.4f\n", improved_bore_capability$Cp))
cat(sprintf("Calculated Cpk (Improved): %.4f\n", improved_bore_capability$Cpk))

# Calculate improved rework rate using 'data'
improved_rework_rate <- mean(data$Rework_Needed == TRUE)
cat(sprintf("Improved Rework Rate: %.2f%%\n", improved_rework_rate * 100))


cat("\n--- Comparison: Initial vs. Improved Process Capability ---\n")
cat(sprintf("Initial Cpk: %.4f, Improved Cpk: %.4f\n", initial_bore_capability$Cpk, improved_bore_capability$Cpk))
cat(sprintf("Initial Cp: %.4f, Improved Cp: %.4f\n", initial_bore_capability$Cp, improved_bore_capability$Cp))
cat(sprintf("Initial Rework Rate: %.2f%%, Improved Rework Rate: %.2f%%\n", initial_rework_rate * 100, improved_rework_rate * 100))

# QM Assessment on Improvement Success (unchanged)
if (improved_bore_capability$Cpk >= 1.33 && improved_rework_rate * 100 < 0.5) {
  cat("\nQM Assessment: **Success!** We've met our Cpk goal (>= 1.33) and our rework rate is now below 0.5%. This process is highly capable and efficient.\n")
} else if (improved_bore_capability$Cpk >= 1.0 && improved_rework_rate * 100 < initial_rework_rate * 100) {
  cat("\nQM Assessment: Significant improvement made, but still some work to do. We're on the right track, but need to push for higher capability.\n")
} else {
  cat("\nQM Assessment: While some changes may have occurred, we haven't met our improvement goals yet. Further analysis or more aggressive improvements may be needed.\n")
}

# --- Control Charts (Post-Improvement) using 'data' ---
cat("\n--- CONTROL Phase: Post-Improvement Control Charts ---\n")

qcc_i_chart_improved <- qcc(data$Bore_Diameter_mm, type = "xbar.one", plot = TRUE,
                            title = "Individual Chart for Bore Diameter (Improved)",
                            ylab = "Bore Diameter (mm)")

qcc_mr_chart_improved <- qcc(data$Bore_Diameter_mm, type = "R.one", plot = TRUE,
                             title = "Moving Range Chart for Bore Diameter (Improved)",
                             ylab = "Moving Range")

# Create temporary data frame for P-chart using 'data'
defect_data_improved_pchart <- data %>%
  mutate(Subgroup = rep(1:num_subgroups, each = subgroup_size, length.out = nrow(data))) %>%
  group_by(Subgroup) %>%
  summarise(
    sample_size = n(),
    num_defects = sum(Rework_Needed == TRUE),
    .groups = 'drop'
  )

cat("\nP-Chart for Rework Needed (Proportion Reworked per Subgroup - Improved):\n")
qcc_p_chart_improved <- qcc(defect_data_improved_pchart$num_defects, sizes = defect_data_improved_pchart$sample_size, type = "p",
                            title = "P-Chart for Rework Needed (Improved)",
                            ylab = "Proportion Reworked")

cat("\nQM Assessment from Control Charts:\n")
cat("- After improvements, these charts should show a stable process, with points consistently within the **new, tighter control limits**.\n")
cat("- Any point falling outside these limits signals a 'special cause' – something has gone wrong, and immediate investigation is required.\n")
cat("- This ongoing monitoring ensures our hard-earned improvements are sustained and allows us to react quickly to any deviations.\n")
