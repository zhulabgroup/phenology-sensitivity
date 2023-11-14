# plot the winter spring with lag
quercus <- read_rds("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/Quercus_winsprtem.rds")

standarized_data <- quercus %>%
  filter(common_name %in% c("northern red oak")) %>% 
  group_by(individual_id) %>%
  mutate_at(vars(flower, lag, leaf, winter_avg_temp, spring_avg_temp), 
            ~ . - mean(.)) %>%
  ungroup()

source("function_plot_sensitive.R")

plots <- plot_sensitive(standarized_data)

# Show combined plot with a shared legend
plot_individual_varibility <- wrap_plots(plots, ncol=2) 





