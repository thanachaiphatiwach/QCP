## Code Block
``` R
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("stringr")
#install.packages("qcc") 

library(dplyr)
library(lubridate)
library(stringr)
library(qcc)

USL <- 90.05
LSL <- 89.95
target_bore <- 90.00

# Calculate Cp and Cpk
calculate_capability <- function(data_column, usl_val, lsl_val){
  process_mean <- mean(data_column, na.rm = TRUE)
  process_sd <- sd(data_column, na.rm= TRUE)

cp <- (usl_val-lsl_val)/(6*process_sd)
cpu <- (usl_val-process_mean)/(3*process_sd)
cpl <- (process_mean-lsl_val)/(3*process_sd)
cpk <- min(cpu,cpl)

return(list(
  mean = process_mean,
  sd = process_sd,
  Cp= cp,
  Cpk = cpk
))
}

bore_capability <- calculate_capability(data$Bore_Diameter_mm, USL, LSL)
cat(sprintf("Initial Process Mean (Bore Diameter): %.4f mm\n", initial_bore_capability$mean))
cat(sprintf("Initial Process Standard Deviation (Bore Diameter): %.4f mm\n", initial_bore_capability$sd))
cat(sprintf("Upper Spec Limit (USL): %.2f mm\n", USL))
cat(sprintf("Lower Spec Limit (LSL): %.2f mm\n", LSL))
cat(sprintf("Calculated Cp (Initial): %.4f\n", initial_bore_capability$Cp))
cat(sprintf("Calculated Cpk (Initial): %.4f\n", initial_bore_capability$Cpk))

# Initial Rework Rate
initial_rework_rate <- mean(data$Rework_Needed == TRUE) # TRUE is 1, FALSE is 0 for mean calculation
cat(sprintf("Initial Rework Rate: %.2f%%\n", initial_rework_rate * 100))

# Interpretation of Initial Cp/Cpk (for the Quality Manager)
if (initial_bore_capability$Cpk < 1.0) {
  cat("\nQM Assessment: Our Cpk is less than 1.0. This means our process is **NOT capable** of meeting specifications. We are producing defects even if the process was perfectly centered. Urgent action is needed!\n")
} else if (initial_bore_capability$Cpk < 1.33) {
  cat("\nQM Assessment: Our Cpk is between 1.0 and 1.33. This means our process is **barely capable**. We're meeting specs, but with very little margin for error. We need to improve this.\n")
} else {
  cat("\nQM Assessment: Our Cpk is 1.33 or higher. This process is generally **capable**. Good baseline performance!\n")
}

if (initial_bore_capability$Cp > initial_bore_capability$Cpk * 1.1) { # Simple heuristic for significant difference
  cat("QM Assessment: Cp is noticeably higher than Cpk. This tells us our process has the *potential* to be better, but it's currently **off-center**. We need to shift the average closer to the target.\n")
} else {
  cat("QM Assessment: Cp and Cpk are close. This suggests our process is reasonably well-centered, but the **overall variation** might be too high.\n")
}

cat("\n--- MEASURE Phase: Control Charts (Initial Process) ---\n")

# Individual (I) Chart for Bore Diameter (Monitors individual measurements)
# Looks for unusual points or trends in the bore diameter values over time.
qcc_i_chart <- qcc(data$Bore_Diameter_mm, type = "xbar.one", plot = TRUE,
                   title = "Individual Chart for Bore Diameter (Initial)",
                   ylab = "Bore Diameter (mm)")

# Moving Range (MR) Chart for Bore Diameter (Monitors variation between consecutive points)
# Helps identify if the variation (spread) in our process is stable.
qcc_mr_chart <- qcc(data$Bore_Diameter_mm, type = "R.one", plot = TRUE,
                    title = "Moving Range Chart for Bore Diameter (Initial)",
                    ylab = "Moving Range")

# P-chart for Rework_Needed: Monitors the proportion of non-conforming items (reworked parts)
# We need to define subgroups for this. Let's use subgroups of 50 consecutive measurements.
subgroup_size <- 50
num_subgroups <- floor(nrow(initial_data) / subgroup_size)

# Create a temporary data frame with subgroups for the P-chart
defect_data_initial_pchart <- initial_data %>%
  mutate(Subgroup = rep(1:num_subgroups, each = subgroup_size, length.out = nrow(initial_data))) %>%
  group_by(Subgroup) %>%
  summarise(
    sample_size = n(),
    num_defects = sum(Rework_Needed == TRUE), # Count how many parts in each subgroup needed rework
    .groups = 'drop' # Important to drop grouping for subsequent operations
  )

cat("\nP-Chart for Rework Needed (Proportion Reworked per Subgroup):\n")
# The P-chart shows if the percentage of rework is in control or varies significantly.
qcc_p_chart <- qcc(defect_data_initial_pchart$num_defects, sizes = defect_data_initial_pchart$sample_size, type = "p",
                   title = "P-Chart for Rework Needed (Initial)",
                   ylab = "Proportion Reworked")

# QM Assessment from Charts
cat("\nQM Assessment from Control Charts:\n")
cat("- Look for any points *outside* the control limits on the I and MR charts. These are 'special causes' and indicate an unstable process.\n")
cat("- Check for any trends (e.g., gradually increasing/decreasing) or unusual patterns. These also suggest instability.\n")
cat("- For the P-chart, are the rework proportions stable? Any spikes or trends?.\n")
cat("- If the process is out of control, our first priority is to stabilize it by finding and eliminating those special causes before focusing on capability.\n")

```
