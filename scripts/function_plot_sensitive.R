plot_sensitive <- function(standarized_data){

# Function to create individual plots
create_plot <- function(data, x_var , y_var, max_abs_x, max_abs_y) {
  
  model <- lm(data[[y_var]] ~ data[[x_var]] - 1)
  
  slope <- coef(model)

  p_value <- broom::tidy(model)$p.value[1]
  
  # Calculate the maximum absolute value in the data for setting y-axis limits
  
  ggplot(data, aes(x = data[[x_var]], y = data[[y_var]], color = common_name)) +
    geom_point(alpha = 0.1, color = "black") +
    geom_abline(intercept = 0, slope = slope, color = "black") + # adding a red line with intercept 0
    geom_text(aes(x = max(data[[x_var]]), y = max(data[[y_var]]), 
                  label = paste("Slope:", round(slope, 2), "\nP-value:", round(p_value, 3))),
              hjust = 1, vjust = 1, color = "black", size = 4) +
    labs(
      x = x_var,
      y = y_var
    ) +
    theme_minimal()+ 
    theme(legend.position="none") + 
    ylim(-max_abs_y, max_abs_y) + 
    xlim(-max_abs_x, max_abs_x) # Set y-axis limits centered at 0
}

x_vars <- c("winter_avg_temp", "spring_avg_temp")
y_vars <- c("leaf", "flower")


combinations <- expand.grid(x=x_vars, y=y_vars)

max_abs_x <- max(abs(standarized_data[, x_vars]))
max_abs_y <- max(abs(standarized_data[, y_vars]))

plots <- lapply(1:nrow(combinations), function(i) {
  create_plot(standarized_data, combinations$x[i], combinations$y[i], max_abs_x, max_abs_y)
})

return(plots)
}

#TEST
#wrap_plots(plots, ncol=2) 