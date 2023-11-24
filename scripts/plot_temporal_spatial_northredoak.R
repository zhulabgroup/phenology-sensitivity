
quercus <- read_rds("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/Quercus_winsprtem.rds")
#quercus <- readRDS("/Volumes/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/Quercus_winsprtem.rds") 

individual_aggre <- quercus %>%
  filter(common_name %in% c("northern red oak")) %>% 
  group_by(individual_id, longitude, latitude, common_name) %>%
  summarise(
    across(
      c(leaf, flower, lag, winter_avg_temp, spring_avg_temp),
      list(
        Mean = ~mean(.),
        Min = ~min(.)-mean(.),
        Max = ~max(.)-mean(.)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%   
  ungroup() %>% 
  mutate_at(vars(flower_Mean, lag_Mean, leaf_Mean, winter_avg_temp_Mean, spring_avg_temp_Mean), 
            ~ . - mean(.)) 

source("scripts/function_plot_spatial_sensitive.R")
spatial_leaf <- plot_spatial_sensitive(individual_aggre, "leaf")
spatial_flower <- plot_spatial_sensitive(individual_aggre, "flower")

standarized_data <- quercus %>%
  filter(common_name %in% c("northern red oak")) %>% 
  group_by(individual_id) %>%
  mutate_at(vars(flower, lag, leaf, winter_avg_temp, spring_avg_temp), 
            ~ . - mean(.)) %>%
  ungroup()

source("scripts/function_plot_temporal_sensitive.R")
temporal_leaf <- plot_temporal_sensitive(standarized_data, "leaf")
temporal_flower <- plot_temporal_sensitive(standarized_data, "flower")

temporal_spatial <- temporal_leaf + spatial_leaf + temporal_flower + spatial_flower +
  plot_layout(ncol = 2)
