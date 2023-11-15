quercus <- read_rds("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/Quercus_winsprtem.rds")


standarized_data <- quercus %>%
  group_by(individual_id) %>%
  filter(n() > 3) %>%  # Filter out groups with only one observation
  mutate_at(vars(flower, lag, leaf, winter_avg_temp, spring_avg_temp), 
            ~ . - mean(.)) %>%
  ungroup()


individual_aggre <- quercus %>%
  group_by(individual_id, longitude, latitude, common_name) %>%
  summarise(across(c(leaf, flower, lag, winter_avg_temp, spring_avg_temp), mean, .names = "{.col}")) %>% 
  group_by(common_name) %>% 
  mutate_at(vars(flower, lag, leaf, winter_avg_temp, spring_avg_temp), 
            ~ . - mean(.)) %>% 
  ungroup()


source("/home/yia/yia_R/npn_flower_leaf_lag/scripts/function_table_sensitive.R")


temporal <- get_stats(standarized_data) %>% 
  rename(tem_slope = slope, tem_p = p)

spatial <- get_stats(individual_aggre) %>% 
  rename(spa_slope = slope, spa_p = p)

spatial_temporal <- inner_join(temporal,spatial,by=c("common_name","pheno"))

species_summary <- spatial_temporal %>% 
  filter(tem_p<0.05 & spa_p<0.05) %>% 
ggplot() + 
  geom_point(aes(x = tem_slope, y = spa_slope, color = factor(pheno))) +  # Convert pheno to factor for categorical color
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +  # Add 1:1 line
  labs(x = "Temporal Slope", y = "Spatial Slope") +  # Label axes
  scale_color_discrete(name = "Pheno")  # Set categorical color scale
  