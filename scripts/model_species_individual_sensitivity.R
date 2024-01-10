# Set the directory containing the RDS files
directory_path <- "/Volumes/seas-zhukai/phenology/phenology_leaf_flower_lag/different_species/"

# Get a list of all RDS files in the directory
rds_files <- list.files(directory_path, pattern = "\\.rds$", full.names = TRUE)
rds_files <- rds_files[-3]
# Initialize an empty data frame
filtered_data <- data.frame()

# Read and combine the RDS files
for (file in rds_files) {
  data <- readRDS(file)
  filtered_data <- rbind(filtered_data,data)
}



# Create an empty data frame to store results
results_table <- data.frame(Threshold1 = integer(), Threshold2 = integer(), P_Value = numeric())

# Loop through different combinations of threshold1 and threshold2
for (threshold1 in 2:7) {
  for (threshold2 in 2:7) {
    # Call the fit_lme_and_extract function with the current thresholds
    p_value <- fit_lme_and_extract(filtered_data, threshold1, threshold2)
    
    # Append the results to the table
    results_table <- rbind(results_table, data.frame(Threshold1 = threshold1, Threshold2 = threshold2, P_Value = p_value))
  }
}

wide_table <- results_table %>%
  pivot_wider(names_from = Threshold1, values_from = P_Value)

# Function to fit LME model and extract estimate and p-value
fit_lme_and_extract <- function(data, threshold1, threshold2) {
  # Filter data for individuals with >= n_threshold observations
  mean_values <- data %>%
    group_by(individual_id, latin_name) %>% 
    summarise(mean_spring_avg_temp = mean(spring_avg_temp),
              n = n()) %>% 
    filter(n >= threshold2) %>% 
    ungroup()
  
  # Filter for species with >= n_threshold individuals
  speciesoi <- mean_values %>%
    group_by(latin_name) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    filter(n >= threshold1)
  
  # Calculate anomalies by subtracting the means
  anomaly_data <- data %>%
    filter(latin_name %in% speciesoi$latin_name) %>%
    right_join(mean_values, by = c("individual_id", "latin_name")) %>%
    mutate(spring_avg_temp_anomaly = spring_avg_temp - mean_spring_avg_temp)
  
  tryCatch({
    # Perform linear mixed-effects regression with random slopes
    lme_result <- lmerTest::lmer(leaf ~ spring_avg_temp + 
                                   (1 | latin_name) + 
                                   (spring_avg_temp | latin_name) + 
                                   spring_avg_temp_anomaly + 
                                   (spring_avg_temp_anomaly | latin_name / individual_id), 
                                 data = anomaly_data)
    
    # Extract estimate and p-value
    estimate <- summary(lme_result)$coefficients["spring_avg_temp_anomaly", "Estimate"]
    p_value <- summary(lme_result)$coefficients["spring_avg_temp_anomaly", "Pr(>|t|)"]
    
    # Adjust p-value based on estimate sign
    if (estimate < 0) {
      p_value <- -p_value
    }
    
    return(p_value)
  }, warning = function(w) {
    # Return NA for warnings (model convergence issues)
    return(NA)
  })
}
