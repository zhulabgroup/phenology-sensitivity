folder_path <- "data/different_species/"
file_to_remove1 <- "data/different_species//climate_list.rds" 
file_to_remove2 <-  "data/different_species//Pinaceae.rds"
file_to_remove3 <-  "data/different_species//Poaceae.rds"

# Get list of RDS files in the folder
rds_files <- list.files(path = folder_path, pattern = "\\.rds$", full.names = TRUE) %>%
  setdiff(file_to_remove1) %>% 
  setdiff(file_to_remove2) %>% 
  setdiff(file_to_remove3) 
  

site_gg <- list()

for (i in seq_along(rds_files) ) {
  oak <- read_rds(rds_files[i])
  
  original_file_name <- tools::file_path_sans_ext(basename(rds_files[i]))
  

quercus <- oak %>% 
  filter(functional_type == "Deciduous broadleaf")

standarized_data <- quercus %>%
  group_by(individual_id) %>%
  mutate_at(vars(flower, leaf, winter_avg_temp, spring_avg_temp), 
            ~ . - mean(.)) %>%
  ungroup()


individual_aggre <- quercus %>%
  group_by(individual_id, common_name) %>%
  summarise(across(c(leaf, flower, winter_avg_temp, spring_avg_temp), mean, .names = "{.col}")) %>% 
  group_by(common_name) %>% 
  mutate_at(vars(flower, leaf, winter_avg_temp, spring_avg_temp), 
            ~ . - mean(.)) %>% 
  ungroup()


source("scripts/function_table_sensitive.R")


temporal <- get_stats(standarized_data) %>% 
  rename(tem_slope = slope, tem_p = p, tem_max = max, tem_min = min)

spatial <- get_stats(individual_aggre) %>% 
  rename(spa_slope = slope, spa_p = p, spa_max = max, spa_min = min)

spatial_temporal <- inner_join(temporal,spatial,by=c("common_name","pheno"))

site_gg[[i]]  <- spatial_temporal %>%
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
  scale_color_discrete(name = "Pheno") +
  
  ggtitle(original_file_name)



}

# pdf("data/byspecies.pdf", width = 8, height = 8 * .618)
# print(site_gg)
# dev.off()
