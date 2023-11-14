# 
library(MASS)

quercus <- read_rds("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/Quercus_winsprtem.rds")

individual_aggre <- quercus %>%
  filter(common_name %in% c("northern red oak")) %>% 
  group_by(individual_id, longitude, latitude, common_name) %>%
  summarise(across(c(leaf, flower, lag, winter_avg_temp, spring_avg_temp), mean, .names = "{.col}")) 

source("function_plot_sensitive.R")

plots <- plot_sensitive(individual_aggre)

# Show combined plot with a shared legend
plot_spatial_northernredoak <- wrap_plots(plots, ncol=2) 
