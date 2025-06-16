cat("\n--- IMPROVE Phase: Simulating New Data with Implemented Improvements ---\n")

num_records_improved <- 1500 # Keeping the same number of records for comparison

# Generate data for the improved process, simulating positive changes
# This new improved data is stored in 'improved_data' temporarily.
improved_data <- data.frame(
  Record_ID = 1:num_records_improved,
  Batch_ID = sample(paste0("B-", str_pad(1:50, 3, pad = "0")), num_records_improved, replace = TRUE),
  Machine_ID = sample(c("CNC-01", "CNC-02", "CNC-03"), num_records_improved, replace = TRUE),
  Timestamp = as_datetime("2024-01-01 08:00:00") + seconds(sort(sample(1:(num_records_improved * 60), num_records_improved, replace = FALSE))),
  
  # *** Key Improvements Simulated Here ***
  Bore_Diameter_mm = rnorm(num_records_improved, mean = target_bore, sd = 0.002),
  Temperature_C = rnorm(num_records_improved, mean = 21.0, sd = 0.5),
  Vibration_Level_mV = rnorm(num_records_improved, mean = 7, sd = 1.0),
  Tool_Wear_Index = pmin(pmax(rnorm(num_records_improved, mean = 0.25, sd = 0.05), 0.1), 0.5),
  
  Material_Hardness_HB = rnorm(num_records_improved, mean = 200, sd = 10),
  Operator_ID = sample(paste0("Op-", 101:105), num_records_improved, replace = TRUE)
)

# Simulate very rare, minor defects due to residual process variation
improved_data <- improved_data %>%
  mutate(
    Defect_Type = case_when(
      Bore_Diameter_mm > (USL + 0.0005) ~ "Oversize Bore",
      Bore_Diameter_mm < (LSL - 0.0005) ~ "Undersize Bore",
      Vibration_Level_mV > 12 & runif(n()) < 0.01 ~ "Surface Roughness Issue",
      Tool_Wear_Index > 0.45 & runif(n()) < 0.005 ~ "Surface Roughness Issue",
      TRUE ~ "None"
    ),
    Rework_Needed = ifelse(Defect_Type != "None" | runif(n()) < 0.0005, TRUE, FALSE),
    Machine_Status = case_when(
      Vibration_Level_mV > 15 ~ "Failure - Stopped",
      Vibration_Level_mV > 10 ~ "Warning - Maintenance Soon",
      Tool_Wear_Index > 0.4 ~ "Warning - Maintenance Soon",
      TRUE ~ "Running Smooth"
    )
  )

# Save the improved data to a new CSV file
write.csv(improved_data, "automotive_manufacturing_improved_data.csv", row.names = FALSE)

cat("New dataset 'automotive_manufacturing_improved_data.csv' created, simulating improvements.\n")
