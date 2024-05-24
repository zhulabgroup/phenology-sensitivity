# Load necessary libraries
library(dplyr)
library(ggplot2)
library(broom)
library(mvoutlier)

# Define the function
analyze_species <- function(data, species_name) {
  # Filter data for the specified species
  species_data <- data %>% filter(species == species_name)
  
  # Identify and remove outliers using aq.plot
  result <- aq.plot(species_data %>% dplyr::select(yeart, doy) %>% as.matrix())
  data_clean <- species_data[!result$outliers, ]
  
  # Fit the linear model
  model <- lm(doy ~ anom + norm, data = data_clean)
  
  # Calculate model summary with confidence intervals
  model_summary <- broom::tidy(model, conf.int = TRUE)
  
  # Extract coefficients and confidence intervals for anom and norm
  coef_anom <- model_summary %>% filter(term == "anom")
  coef_norm <- model_summary %>% filter(term == "norm")
  
  # Calculate R-squared
  r_squared <- summary(model)$r.squared
  
  # Calculate means of anom, norm, and doy
  mean_yeart <- mean(data_clean$yeart, na.rm = TRUE)
  mean_doy <- mean(data_clean$doy, na.rm = TRUE)
  
  # Calculate the adjusted intercepts for the center of the figure
  intercept_anom <- mean_doy - coef(model)["anom"] * mean_yeart
  
  # Generate the plot
  plot <- ggplot(data_clean, aes(x = yeart, y = doy)) +
    geom_point(alpha = 0.5) +
    geom_abline(aes(intercept = intercept_anom, slope = coef(model)["anom"], color = "Temporal")) +
    geom_abline(aes(intercept = coef(model)[1], slope = coef(model)["norm"], color = "Spatial")) +
    labs(title = species_name, x = "Spring temperature", y = "Day of Year") +
    annotate("text", x = min(data_clean$yeart), y = min(data_clean$doy), 
             label = sprintf("Temporal CI: [%0.2f, %0.2f]\nSpatial CI: [%0.2f, %0.2f]\nR² = %0.2f", 
                             coef_anom$conf.low, coef_anom$conf.high, 
                             coef_norm$conf.low, coef_norm$conf.high, 
                             r_squared),
             hjust = 0, vjust = 0, size = 5, color = "black")
  
  # Create a summary row
  summary_row <- tibble(
    species = species_name,
    anom_estimate = coef_anom$estimate,
    anom_conf_low = coef_anom$conf.low,
    anom_conf_high = coef_anom$conf.high,
    norm_estimate = coef_norm$estimate,
    norm_conf_low = coef_norm$conf.low,
    norm_conf_high = coef_norm$conf.high,
    r_squared = r_squared
  )
  
  return(list(plot = plot, summary = summary_row))
}

# Apply the function to each species and store the results
results <- list()
unique_species <- unique(temperature_data$species)

for (species_name in unique_species) {
  results[[species_name]] <- analyze_species(temperature_data, species_name)
}

# Combine all summary rows into a single data frame
summary_results <- bind_rows(lapply(results, function(res) res$summary))

# Print the summary results
print(summary_results)

# Save all plots to a single PDF file
pdf("species_plots.pdf", width = 8, height = 6)
for (species_name in unique_species) {
  print(results[[species_name]]$plot)
}
dev.off()

write.csv(summary_results, "species_summary.csv", row.names = FALSE)
