# Define the function
analyze_species <- function(data, species_name) {
  # Filter data for the specified species
  species_data <- data %>% filter(species == species_name)
  
  # Fit the linear model
  model <- MASS::rlm(doy ~ anom + norm, data = species_data,  maxit = 30)
  
  cov_matrix <- vcov(model)
  
  # Get the variances and covariance needed
  var_norm <- cov_matrix["norm", "norm"]
  var_anom <- cov_matrix["anom", "anom"]
  cov_norm_anom <- cov_matrix["norm", "anom"]
  
  # Compute the variance of the difference between the coefficients
  var_diff <- var_norm + var_anom - 2 * cov_norm_anom
 
  lh_test <- car::linearHypothesis(model, "norm - anom = 0")
  
  # Extract the p-value
  p_value <- lh_test$`Pr(>F)`[2]
  
  # Determine the conclusion based on the p-value
  equal <- ifelse(p_value < 0.05, 0, 1) # 0 = reject null, this is a significant differences, 1 = fail to reject null
  
  # Calculate model summary with confidence intervals
  model_summary <- broom::tidy(model, conf.int = TRUE)
  
  # Extract coefficients and confidence intervals for anom and norm
  coef_anom <- model_summary %>% filter(term == "anom")
  coef_norm <- model_summary %>% filter(term == "norm")
  
  # Calculate R-squared
  residual <- summary(model)$sigma
  
  # Calculate means of anom, norm, and doy
  mean_yeart <- mean(species_data$yeart, na.rm = TRUE)
  mean_doy <- mean(species_data$doy, na.rm = TRUE)
  
  # Calculate the adjusted intercepts for the center of the figure
  intercept_anom <- mean_doy - coef(model)["anom"] * mean_yeart
  
  # Generate the plot
  plot <- ggplot(species_data, aes(x = yeart, y = doy)) +
    geom_point(aes(alpha = model$w)) +
    geom_abline(aes(intercept = intercept_anom, slope = coef(model)["anom"], color = "Temporal")) +
    geom_abline(aes(intercept = coef(model)[1], slope = coef(model)["norm"], color = "Spatial")) +
    labs(title = species_name, 
         x = "Spring temperature", 
         y = "Days since Nov 1st",
         shape = "Equal",
         alpha = "Weight",
         color = "Type"
         ) +
    annotate("text", x = min(species_data$yeart), y = min(species_data$doy), 
             label = sprintf("Temporal CI: [%0.2f, %0.2f]\nSpatial CI: [%0.2f, %0.2f]\nResidual standard error = %0.2f\nEqual = %d\nVariance of difference = %0.2f", 
                             coef_anom$conf.low, coef_anom$conf.high, 
                             coef_norm$conf.low, coef_norm$conf.high, 
                             residual, equal, var_diff),
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
    residual = residual,
    equal = equal,
    diff_var = var_diff
  )
  
  return(list(plot = plot, summary = summary_row))
}



