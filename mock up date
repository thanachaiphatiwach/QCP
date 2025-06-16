## Code block
```R
#Install necessary packages if you haven't already
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("stringr")
#install.packages("stats") # For rnorm, etc.

library(dplyr)
library(lubridate)
library(stringr)

set.seed(123) # for reproducibility

num_records <- 1500 # More than 1000 records

# Define specifications for Bore_Diameter_mm
target_bore <- 90.00
usl_bore <- 90.05
lsl_bore <- 89.95
tolerance <- (usl_bore - lsl_bore) / 2

# Generate data
data <- data.frame(
  Record_ID = 1:num_records,
  Batch_ID = sample(paste0("B-", str_pad(1:50, 3, pad = "0")), num_records, replace = TRUE),
  Machine_ID = sample(c("CNC-01", "CNC-02", "CNC-03"), num_records, replace = TRUE),
  Timestamp = as_datetime("2024-01-01 08:00:00") + seconds(sort(sample(1:(num_records * 60), num_records, replace = FALSE))), # Spaced out over time
  
  # Bore Diameter - introduce some variation and occasional outliers/shifts
  Bore_Diameter_mm = rnorm(num_records, mean = target_bore, sd = 0.01), # Base variation
  Temperature_C = rnorm(num_records, mean = 22, sd = 1.5),
  Vibration_Level_mV = rnorm(num_records, mean = 15, sd = 3),
  Tool_Wear_Index = pmin(pmax(rnorm(num_records, mean = 0.5, sd = 0.2), 0.1), 1.0), # Bounded between 0.1 and 1.0
  Material_Hardness_HB = rnorm(num_records, mean = 200, sd = 10),
  Operator_ID = sample(paste0("Op-", 101:105), num_records, replace = TRUE)
)

# Introduce some controlled "defects" and shifts for ML/SPC
# Example: machine 02 might have a tendency for slightly oversized bores
data <- data %>%
  mutate(
    Bore_Diameter_mm = ifelse(Machine_ID == "CNC-02" & runif(n()) < 0.1, Bore_Diameter_mm + 0.03, Bore_Diameter_mm), # 10% chance of being slightly oversized on CNC-02
    Bore_Diameter_mm = ifelse(Machine_ID == "CNC-03" & runif(n()) < 0.05, Bore_Diameter_mm - 0.02, Bore_Diameter_mm), # 5% chance of being slightly undersized on CNC-03
    
    # Introduce some vibration spikes indicating potential issues
    Vibration_Level_mV = ifelse(Tool_Wear_Index > 0.8 & runif(n()) < 0.2, Vibration_Level_mV * 1.5, Vibration_Level_mV), # Higher vibration with high tool wear
    Vibration_Level_mV = ifelse(Record_ID %in% sample(1:num_records, 20), Vibration_Level_mV * 2, Vibration_Level_mV) # Random spikes
  )

# Define Defect_Type and Rework_Needed based on Bore_Diameter_mm and other factors
data <- data %>%
  mutate(
    Defect_Type = case_when(
      Bore_Diameter_mm > usl_bore ~ "Oversize Bore",
      Bore_Diameter_mm < lsl_bore ~ "Undersize Bore",
      Vibration_Level_mV > 25 & runif(n()) < 0.3 ~ "Surface Roughness Issue", # High vibration can cause surface issues
      Tool_Wear_Index > 0.95 & runif(n()) < 0.1 ~ "Surface Roughness Issue",
      TRUE ~ "None"
    ),
    Rework_Needed = ifelse(Defect_Type != "None" | runif(n()) < 0.02, TRUE, FALSE), # Rework if defect or small random chance
    Machine_Status = case_when(
      Vibration_Level_mV > 30 ~ "Failure - Stopped",
      Vibration_Level_mV > 20 ~ "Warning - Maintenance Soon",
      Tool_Wear_Index > 0.9 ~ "Warning - Maintenance Soon",
      TRUE ~ "Running Smooth"
    )
  )

# Final check of data structure
str(data)
summary(data)
head(data)

# Save to CSV for easy use
write.csv(data, "automotive_manufacturing_data.csv", row.names = FALSE)
```
