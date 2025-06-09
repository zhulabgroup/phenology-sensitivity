# fit the model

temperature_data <- read.csv(.path$temperature_data)


## get data for model by requiring 10 observations for each parameters --------

temperature_data_model <- temperature_data %>%
  group_by(species, dataset) %>%
  # filter(n_distinct(doy) > 10) %>%  # hard to justify
  filter(n_distinct(anom) > 10) %>%  # Use n_distinct() for distinct counting
  filter(n_distinct(norm) > 10) %>%  # Use n_distinct() for distinct counting
  filter(n_distinct(doy, norm, anom) > 30) %>%  # Use n_distinct() for distinct counting
  ungroup() 


## fit the model for each species-dataset combination ----------------------

# Get unique combinations of species and dataset
unique_combos <- temperature_data_model %>%
  distinct(species, dataset)

# Define function to analyze each species-dataset combination
analyze_species_dataset <- function(data, species_name, dataset_name) {
  # Filter data for the specified species and dataset
  filtered_data <- data %>%
    filter(species == species_name, dataset == dataset_name)
  
  # Fit the linear model
  model <- MASS::rlm(doy ~ anom + norm, data = filtered_data, maxit = 30)
  
  cov_matrix <- vcov(model)

  # Get the variances and covariance needed
  var_norm <- cov_matrix["norm", "norm"]
  var_anom <- cov_matrix["anom", "anom"]
  cov_norm_anom <- cov_matrix["norm", "anom"]

  # Compute the variance of the difference between the coefficients
  var_diff <- var_norm + var_anom - 2 * cov_norm_anom
  #
  lh_test <- car::linearHypothesis(model, "norm - anom = 0")

  # Extract the p-value
  p_value <- lh_test$`Pr(>F)`[2]

  # Determine the conclusion based on the p-value
  equal <- ifelse(p_value < 0.05, 0, 1) # 0 = reject null, this is a significant differences, 1 = fail to reject null

  # Get model summary
  model_summary <- broom::tidy(model, conf.int = TRUE)
  
  # Extract coefficients
  coef_anom <- model_summary %>% filter(term == "anom")
  coef_norm <- model_summary %>% filter(term == "norm")
  
  # Residual standard error
  residual <- summary(model)$sigma
  
  # Return as a tibble row
  tibble(
    species = species_name,
    genus = unique(filtered_data$genus),
    taxa = unique(filtered_data$taxa),
    dataset = dataset_name,
    anom_estimate = coef_anom$estimate,
    anom_conf_low = coef_anom$conf.low,
    anom_conf_high = coef_anom$conf.high,
    norm_estimate = coef_norm$estimate,
    norm_conf_low = coef_norm$conf.low,
    norm_conf_high = coef_norm$conf.high,
    equal = equal,
    residual = residual
  )
}

# Apply function across all combinations
results_list <- list()

for (i in seq_len(nrow(unique_combos))) {
  species_name <- unique_combos$species[i]
  dataset_name <- unique_combos$dataset[i]
  
  key <- paste(species_name, dataset_name, sep = "_")
  results_list[[key]] <- analyze_species_dataset(temperature_data_model, species_name, dataset_name)
}

# Combine all into a single data frame
final_results <- bind_rows(results_list)



write.csv(final_results, .path$byspecies_summary, row.names = FALSE)

