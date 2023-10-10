# 
library(MASS)

quercus <- read_rds("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/Quercus_winsprtem.rds")

individual_aggre <- quercus %>%
  filter(common_name %in% c("northern red oak")) %>% 
  group_by(individual_id, longitude, latitude, common_name) %>%
  summarise(across(c(leaf, flower, lag, winter_avg_temp, spring_avg_temp), mean, .names = "{.col}")) 


# Function to create individual plots
create_plot <- function(data, x_var, y_var) {
  slope <- coef(rlm(data[[y_var]] ~ data[[x_var]]))[2]
  
  ggplot(data, aes(x = data[[x_var]], y = data[[y_var]], color = common_name)) +
    geom_smooth(method = "rlm", se = FALSE) +
    geom_point(alpha = 0.5) +
    geom_text(aes(x = max(data[[x_var]]), y = max(data[[y_var]]), label = paste("Slope:", round(slope, 2))),
              hjust = 1, vjust = 1, color = "red", size = 4) + # Add the slope label
    labs(
      x = x_var,
      y = y_var
    ) +
    theme_minimal() + 
    theme(legend.position="none") # remove individual plot legends
}

x_vars <- c("winter_avg_temp", "spring_avg_temp")
y_vars <- c("leaf", "flower","lag")

combinations <- expand.grid(x=x_vars, y=y_vars)

plots <- lapply(1:nrow(combinations), function(i) {
  create_plot(individual_aggre, combinations$x[i], combinations$y[i])
})



# Show combined plot with a shared legend
plot_spatial_northernredoak <- wrap_plots(plots, ncol=2) 
