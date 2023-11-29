library(purrr)

get_stats <- function(standarized_data) {
  result_df <- standarized_data %>%
    group_by(common_name) %>%
    nest() %>%
    mutate(results = map(data, plot_sensitive)) %>%
    unnest(results) %>%
    ungroup() %>%
    select(common_name, slope, p, max, min)
  
  return(result_df)
}

plot_sensitive <- function(species_data) {
  result_table <- data.frame(slope = numeric(0), p = numeric(0), 
                             max = numeric(0), min = numeric(0))

  result_table <- create_plot(species_data, "spring_avg_temp", "first_yes_doy")
  
  return(result_table)
}


create_plot <- function(data, x_var, y_var) {
  model <- lm(data[[y_var]] ~ data[[x_var]] - 1)
  slope <- coef(model)
  p_value <- broom::tidy(model)$p.value[1]
  
  # Calculate the confidence interval for the slope
  conf_interval <- confint(model, level = 0.95)  # 95% confidence interval
  
  # Extract the lower and upper bounds of the confidence interval
  lower_bound <- conf_interval[1]
  upper_bound <- conf_interval[2]
  
  # Create a data frame with the results
  result <- data.frame(
    pheno = y_var,
    slope = round(slope, 2),
    p = round(p_value, 3),
    min = round(lower_bound, 2),  # Rounded to two decimal places
    max = round(upper_bound, 2)   # Rounded to two decimal places
  )
  
  return(result)
}

# 