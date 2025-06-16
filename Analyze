cat("\n--- ANALYZE Phase: Hypothesis Testing for Root Causes ---\n")

# Helper function to print hypothesis test results clearly (unchanged)
print_htest_result <- function(test_name, p_value, alpha = 0.05, conclusion_type = "difference/relationship") {
  cat(sprintf("\nTest: %s\n", test_name))
  cat(sprintf("P-value: %.4f\n", p_value))
  if (p_value < alpha) {
    cat(sprintf("Conclusion: P-value < %.2f. **Reject the Null Hypothesis.** There is a statistically significant %s.\n", alpha, conclusion_type))
  } else {
    cat(sprintf("Conclusion: P-value >= %.2f. **Fail to Reject the Null Hypothesis.** There is no statistically significant %s based on this data.\n", alpha, conclusion_type))
  }
}

# --- A. Influence of Categorical Factors on Bore_Diameter_mm (ANOVA / F-test) ---
cat("\n--- Analyzing Bore Diameter vs. Categorical Factors (Machine, Operator) ---\n")

# ANOVA for Machine_ID's influence on Bore_Diameter_mm using 'data'
anova_machine <- aov(Bore_Diameter_mm ~ Machine_ID, data = data)
summary_anova_machine <- summary(anova_machine)
print_htest_result("ANOVA (Bore_Diameter_mm by Machine_ID)", summary_anova_machine[[1]]$`Pr(>F)`[1], conclusion_type = "difference in means between machines")
if (summary_anova_machine[[1]]$`Pr(>F)`[1] < 0.05) {
  cat("  -> Further analysis (Tukey HSD) to see *which* machines differ:\n")
  print(TukeyHSD(anova_machine))
}

# ANOVA for Operator_ID's influence on Bore_Diameter_mm using 'data'
anova_operator <- aov(Bore_Diameter_mm ~ Operator_ID, data = data)
summary_anova_operator <- summary(anova_operator)
print_htest_result("ANOVA (Bore_Diameter_mm by Operator_ID)", summary_anova_operator[[1]]$`Pr(>F)`[1], conclusion_type = "difference in means between operators")
if (summary_anova_operator[[1]]$`Pr(>F)`[1] < 0.05) {
  cat("  -> Further analysis (Tukey HSD) to see *which* operators differ:\n")
  print(TukeyHSD(anova_operator))
}


# --- B. Influence of Continuous Factors on Bore_Diameter_mm (Linear Regression / t-test on coefficients) ---
cat("\n--- Analyzing Bore Diameter vs. Continuous Factors (Temperature, Vibration, Tool Wear, Hardness) ---\n")

# Linear Regression for Temperature_C using 'data'
lm_temp <- lm(Bore_Diameter_mm ~ Temperature_C, data = data)
summary_lm_temp <- summary(lm_temp)
print_htest_result("Linear Regression (Bore_Diameter_mm ~ Temperature_C)", summary_lm_temp$coefficients["Temperature_C", "Pr(>|t|)"], conclusion_type = "linear relationship with Temperature_C")

# Linear Regression for Vibration_Level_mV using 'data'
lm_vibration <- lm(Bore_Diameter_mm ~ Vibration_Level_mV, data = data)
summary_lm_vibration <- summary(lm_vibration)
print_htest_result("Linear Regression (Bore_Diameter_mm ~ Vibration_Level_mV)", summary_lm_vibration$coefficients["Vibration_Level_mV", "Pr(>|t|)"], conclusion_type = "linear relationship with Vibration_Level_mV")

# Linear Regression for Tool_Wear_Index using 'data'
lm_toolwear <- lm(Bore_Diameter_mm ~ Tool_Wear_Index, data = data)
summary_lm_toolwear <- summary(lm_toolwear)
print_htest_result("Linear Regression (Bore_Diameter_mm ~ Tool_Wear_Index)", summary_lm_toolwear$coefficients["Tool_Wear_Index", "Pr(>|t|)"], conclusion_type = "linear relationship with Tool_Wear_Index")

# Linear Regression for Material_Hardness_HB using 'data'
lm_hardness <- lm(Bore_Diameter_mm ~ Material_Hardness_HB, data = data)
summary_lm_hardness <- summary(lm_hardness)
print_htest_result("Linear Regression (Bore_Diameter_mm ~ Material_Hardness_HB)", summary_lm_hardness$coefficients["Material_Hardness_HB", "Pr(>|t|)"], conclusion_type = "linear relationship with Material_Hardness_HB")


# --- C. Factors Influencing Rework_Needed (Defect Occurrence) ---
cat("\n--- Analyzing Rework_Needed vs. Other Factors ---\n")

# Chi-squared Test: Rework_Needed vs Operator_ID using 'data'
contingency_table_rework_op <- table(data$Operator_ID, data$Rework_Needed)
chisq_rework_op <- chisq.test(contingency_table_rework_op)
print_htest_result("Chi-squared Test (Rework_Needed vs Operator_ID)", chisq_rework_op$p.value, conclusion_type = "association between rework and operator")

# Logistic Regression using 'data'
data$Rework_Needed_Factor <- as.factor(data$Rework_Needed)
logistic_model <- glm(Rework_Needed_Factor ~ Bore_Diameter_mm + Vibration_Level_mV + Tool_Wear_Index + Temperature_C + Machine_ID,
                      data = data, family = "binomial")
summary_logistic_model <- summary(logistic_model)
cat("\nLogistic Regression Summary (Rework_Needed ~ Bore_Diameter_mm + ...):\n")
print(summary_logistic_model)
cat("\nQM Assessment for Logistic Regression: Look at the 'Pr(>|z|)' column. Values less than 0.05 mean that factor is a significant predictor of rework.\n")
