quercus_leaf <- read_rds("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/neon/Quercus_leaf_winsprtem_all.rds")
quercus_flower <- read_rds("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/neon/Quercus_flower_winsprtem_all.rds")

test <- quercus_leaf %>% 
  group_by(common_name) %>% 
  summarise(count = n()) %>% 
  filter(count>10)


quercus_leaf <- quercus_leaf %>% 
  filter(common_name %in% test$common_name)

quercus_flower <- quercus_flower %>% 
  filter(common_name %in% test$common_name)

standarized_data_leaf <- quercus_leaf %>%
  group_by(individual_id) %>%
  mutate_at(vars(first_yes_doy, winter_avg_temp, spring_avg_temp), 
            ~ . - mean(.)) %>%
  ungroup()

standarized_data_flower <- quercus_flower %>%
  group_by(individual_id) %>%
  mutate_at(vars(first_yes_doy, winter_avg_temp, spring_avg_temp), 
            ~ . - mean(.)) %>%
  ungroup()

individual_aggre_leaf <- quercus_leaf %>%
  group_by(longitude, latitude, common_name) %>%
  summarise(across(c(first_yes_doy, winter_avg_temp, spring_avg_temp), mean, .names = "{.col}")) %>% 
  group_by(common_name) %>% 
  mutate_at(vars(first_yes_doy, winter_avg_temp, spring_avg_temp), 
            ~ . - mean(.)) %>% 
  ungroup()

individual_aggre_flower <- quercus_flower %>%
  group_by(longitude, latitude, common_name) %>%
  summarise(across(c(first_yes_doy, winter_avg_temp, spring_avg_temp), mean, .names = "{.col}")) %>% 
  group_by(common_name) %>% 
  mutate_at(vars(first_yes_doy, winter_avg_temp, spring_avg_temp), 
            ~ . - mean(.)) %>% 
  ungroup()


source("function_table_sensitive_sepa.R")


temporal_leaf <- get_stats(standarized_data_leaf) %>% 
  rename(tem_slope = slope, tem_p = p, tem_max = max, tem_min = min)

temporal_flower <- get_stats(standarized_data_flower) %>% 
  rename(tem_slope = slope, tem_p = p, tem_max = max, tem_min = min)

spatial_leaf <- get_stats(individual_aggre_leaf) %>% 
  rename(spa_slope = slope, spa_p = p, spa_max = max, spa_min = min)

spatial_flower <- get_stats(individual_aggre_flower) %>% 
  rename(spa_slope = slope, spa_p = p, spa_max = max, spa_min = min)

spatial_temporal_leaf <- inner_join(temporal_leaf,spatial_leaf,by= "common_name") %>% 
  mutate(pheno = "leaf")

spatial_temporal_flower <- inner_join(temporal_flower,spatial_flower,by= "common_name") %>% 
  mutate(pheno = "flower")

spatial_temporal <- rbind(spatial_temporal_leaf, spatial_temporal_flower)

species_summary <- spatial_temporal %>%
  ggplot() + 
  geom_point(aes(x = tem_slope, y = spa_slope, color = factor(pheno), text = common_name)) +
  
  # Error bars for p < 0.05
  geom_errorbarh(
    data = subset(spatial_temporal, tem_p < 0.05),
    aes(xmin = tem_min, xmax = tem_max, y = spa_slope),
    alpha = 0.2,
    size = 1  # Thicker for significant
  ) +
  geom_errorbar(
    data = subset(spatial_temporal, spa_p < 0.05),
    aes(ymin = spa_min, ymax = spa_max, x = tem_slope),
    alpha = 0.2,
    size = 1
  ) +
  
  # Error bars for p >= 0.05
  geom_errorbarh(
    data = subset(spatial_temporal, tem_p >= 0.05),
    aes(xmin = tem_min, xmax = tem_max, y = spa_slope),
    alpha = 0.2,
    size = 0.25  # Thinner for non-significant
  ) +
  geom_errorbar(
    data = subset(spatial_temporal, spa_p >= 0.05),
    aes(ymin = spa_min, ymax = spa_max, x = tem_slope),
    alpha = 0.2,
    size = 0.25
  ) +
  
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(x = "Temporal Slope", y = "Spatial Slope") 


