quercus <- read_rds("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/Quercus_winsprtem.rds")


standarized_data <- quercus %>%
  group_by(individual_id) %>%
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


source("scripts/function_table_sensitive.R")


temporal <- get_stats(standarized_data) %>% 
  rename(tem_slope = slope, tem_p = p, tem_max = max, tem_min = min)

spatial <- get_stats(individual_aggre) %>% 
  rename(spa_slope = slope, spa_p = p, spa_max = max, spa_min = min)

spatial_temporal <- inner_join(temporal,spatial,by=c("common_name","pheno"))

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
  labs(x = "Temporal Slope", y = "Spatial Slope") +
  scale_color_discrete(name = "Pheno")

ggplotly(species_summary)

  plotly