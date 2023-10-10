# plot the winter spring with lag
quercus <- read_rds("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/Quercus_winsprtem.rds")

individual_aggre <- quercus %>%
  group_by(individual_id, longitude, latitude, common_name) %>%
  summarise(across(c(leaf, flower, lag, winter_avg_temp, spring_avg_temp), mean, .names = "{.col}")) 

# Function to create individual plots
create_plot <- function(data, x_var = c("winter_avg_temp", "spring_avg_temp"), y_var = c("leaf", "flower","lag")) {
  ggplot(data, aes(x = data[[x_var]], y = data[[y_var]], color = common_name)) +
    geom_point(alpha = 0.5) +
    labs(
      x = x_var,
      y = y_var
    ) +
    theme_minimal() + 
    theme(legend.position="none") # remove individual plot legends
}


combinations <- expand.grid(x=x_vars, y=y_vars)

plots <- lapply(1:nrow(combinations), function(i) {
  create_plot(individual_aggre, combinations$x[i], combinations$y[i])
})

# Combine plots using patchwork
combined_plot <- wrap_plots(plots, ncol=4)

# Show combined plot with a shared legend
combined_plot + plot_layout(guides = "collect") & theme(legend.position = "right")
