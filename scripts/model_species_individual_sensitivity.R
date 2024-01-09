maple <- read_rds("/Volumes/seas-zhukai/phenology/phenology_leaf_flower_lag/different_species/Acer.rds")
oak <- read_rds("/Volumes/seas-zhukai/phenology/phenology_leaf_flower_lag/different_species/Quercus.rds")

phase <- "flower"
# Filter data for "Acer rubrum" and group by "individual_id"
filtered_data <-  rbind(maple, oak)
  # filter(latin_name == "Acer saccharum") %>%

# Calculate the mean for each "individual_id"
mean_values <- filtered_data %>%
  group_by(individual_id, latin_name) %>% 
  summarise(mean_spring_avg_temp = mean(spring_avg_temp),
            n = n()) %>% 
  filter(n >= 5) %>% 
  ungroup()

nrow(mean_values)
speciesoi <- mean_values %>% group_by(latin_name) %>% summarise(n = n()) %>% arrange(desc(n)) %>% filter(n>=5)

# Calculate anomalies by subtracting the means
anomaly_data <- filtered_data %>%
  filter(latin_name %in% speciesoi$latin_name) %>%
  right_join(mean_values, by = c ("individual_id", "latin_name")) %>%
  mutate(spring_avg_temp_anomaly = spring_avg_temp - mean_spring_avg_temp) %>% 
  mutate(pheno = !!sym(phase))

# Perform linear regression on the anomaly data
lm_result <- lm( pheno ~  spring_avg_temp + spring_avg_temp_anomaly , data = anomaly_data)

# View the regression summary
summary(lm_result)

# Perform linear mixed-effects regression with random slopes
lme_result <- lmerTest::lmer(flower ~ spring_avg_temp  + (1 | latin_name) + (spring_avg_temp | latin_name) + spring_avg_temp_anomaly + (spring_avg_temp_anomaly | latin_name / individual_id), data = anomaly_data)

# View the regression summary
summary(lme_result)

