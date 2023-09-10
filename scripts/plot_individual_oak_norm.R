# plot the winter spring with lag
quercus <- read_rds("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/Quercus_winsprtem.rds")

standarized_data <- quercus %>%
  group_by(individual_id) %>%
  mutate_at(vars(flower, lag, leaf, winter_avg_temp, spring_avg_temp), 
            ~ . - mean(.)) %>%
  ungroup()


library(patchwork)


  # Function to create individual plots
create_plot <- function(data, x_var, y_var) {
    ggplot(data, aes_string(x = x_var, y = y_var)) +
      geom_point(alpha = 0.1) +
      # geom_abline(intercept = 0, slope = 1, color = "red") + # adding a red line with slope 1 and intercept 0
      labs(
        x = x_var,
        y = y_var
      ) +
      theme_minimal()
}
  
  
# Example usage:
x_vars <- c("winter_avg_temp",  "winter_avg_temp",  "winter_avg_temp","spring_avg_temp","spring_avg_temp", "spring_avg_temp")
y_vars <- c("lag",  "leaf",  "flower", "lag","leaf","flower")

plots <- lapply(1:length(x_vars), function(i) create_plot(standarized_data, x_vars[i], y_vars[i]))


plot_individual_varibility <- wrap_plots(plots)


