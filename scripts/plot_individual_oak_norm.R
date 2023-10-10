# plot the winter spring with lag
quercus <- read_rds("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/Quercus_winsprtem.rds")

standarized_data <- quercus %>%
  group_by(individual_id) %>%
  mutate_at(vars(flower, lag, leaf, winter_avg_temp, spring_avg_temp), 
            ~ . - mean(.)) %>%
  ungroup()

x_vars <- c("winter_avg_temp", "spring_avg_temp")
y_vars <- c("leaf", "flower","lag")





combined_plot <- wrap_plots(plots, ncol=2)

# Show combined plot with a shared legend
combined_plot + plot_layout(guides = "collect") & theme(legend.position = "right")
