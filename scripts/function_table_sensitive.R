get_stats <- function(standarized_data){
  unique_common_names <- unique(standarized_data$common_name)
  
  # Initialize an empty list to store results
  result_list <- list()
  
  # Loop through each common_name
  for (name in unique_common_names) {
    data_subset <- standarized_data %>%
      filter(common_name == name)
    
    result <- plot_sensitive(data_subset)
    
    # Store the result in the list
    result_list[[name]] <- result
  }
  # Unnest the list of results into a single data frame
  result_df <- result_list %>%
    bind_rows(.id = "common_name")
  
  # Print the resulting data frame
  return(result_df)
  
}

plot_sensitive <- function(species_data){

  result_table <- data.frame(pheno = character(0), slope = numeric(0), p = numeric(0))
    
  x_vars <- c("winter_avg_temp", "spring_avg_temp")
  y_vars <- c("leaf", "flower")
  combinations <- expand.grid(x=x_vars, y=y_vars)
  
  # Loop through combinations and calculate slopes and p-values
  for (i in 1:nrow(combinations)) {
    result <- create_plot(species_data, combinations$x[i], combinations$y[i])
    result_table <- rbind(result_table, result)
  }
  return(result_table)
}

# Function to create individual plots
create_plot <- function(data, x_var , y_var, max_abs_x, max_abs_y) {
  model <- lm(data[[y_var]] ~ data[[x_var]] - 1)
  slope <- coef(model)
  p_value <- broom::tidy(model)$p.value[1]
  return(data.frame(pheno = paste0(x_var, "-", y_var), slope = round(slope, 2), p = round(p_value, 3)))
}

