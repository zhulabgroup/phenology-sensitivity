# plot the winter spring with lag
quercus <- read_rds("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/Quercus_winsprtem.rds")

standarized_data <- quercus %>%
  group_by(individual_id) %>%
  filter(n() > 1) %>%
  mutate_at(vars(flower, lag, leaf, winter_avg_temp, spring_avg_temp), 
            ~ . - mean(.)) %>%
  ungroup()

# Function to create individual plots
create_plot <- function(data, x_var, y_var, max_abs_x, max_abs_y) {
  
  model <- lm(data[[y_var]] ~ data[[x_var]])
  slope <- coef(model)[2]
  intercept <- coef(model)[1]
  
  # Extract p-value of the slope from the model's summary
  p_value_slope <- summary(model)$coefficients[2, 4]
  
  ggplot(data, aes(x = data[[x_var]], y = data[[y_var]], color = common_name)) +
    geom_point(alpha = 0.1) +
    geom_abline(intercept = intercept, slope = slope, color = "red") + # adding a red line with intercept 0
    geom_text(aes(x = max(data[[x_var]]), y = max(data[[y_var]]), 
                  label = paste("Slope:", round(slope, 2), "\nIntercept:", round(intercept, 2), "\nP-value:", round(p_value_slope, 3))),
              hjust = 1, vjust = 1, color = "red", size = 4) +
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

# Show combined plot with a shared legend
plot_individual_varibility <- wrap_plots(plots, ncol=2) + plot_layout(guides = "collect") & theme(legend.position = "right")


