directory_path <- "/Volumes/seas-zhukai/phenology/phenology_leaf_flower_lag/different_species/"

# Get a list of all RDS files in the directory
rds_files <- list.files(directory_path, pattern = "\\.rds$", full.names = TRUE)
rds_files <- rds_files[-3]
# Initialize an empty data frame
filtered_data <- data.frame()

# Read and combine the RDS files
for (file in rds_files) {
  data <- readRDS(file)
  filtered_data <- rbind(filtered_data,data)
}


mean_values <- filtered_data  %>% 
  filter(functional_type %in% c("Deciduous broadleaf", "Deciduous conifer")) %>%
  group_by(individual_id, latin_name) %>% 
  summarise(mean_spring_avg_temp = mean(spring_avg_temp),
            mean_leaf = mean(leaf),
            n = n()) %>% 
  filter(n >= 3) %>% 
  ungroup()

# Filter for species with >= n_threshold individuals
speciesoi <- mean_values %>%
  group_by(latin_name) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  filter(n >= 3)

# Calculate anomalies by subtracting the means
anomaly_data <- filtered_data %>%
  right_join(mean_values, by = c("individual_id", "latin_name")) %>% #also filtered individual
  filter(latin_name %in% speciesoi$latin_name) %>%
  mutate(spring_avg_temp_anomaly = spring_avg_temp - mean_spring_avg_temp,
         leaf_anomaly = leaf - mean_leaf,
         interaction = spring_avg_temp_anomaly*spring_avg_temp)

# Load the required libraries
library(lme4)

# Fit the OLS linear model
ols_model <- lm(leaf ~ spring_avg_temp, data = anomaly_data)

# Fit the mixed-effects model (assuming 'Group' is the grouping variable)
mixed_model <- lmer(leaf ~ spring_avg_temp + (1|latin_name) + (spring_avg_temp|latin_name), data = anomaly_data)

anomaly_data$prepop <- predict(mixed_model,re.form=NA)  ## population level
anomaly_data$predindi <- predict(mixed_model) ## individual level


# Create a scatterplot with raw data points and model lines
scatterplot <- ggplot(anomaly_data, aes(x = spring_avg_temp, y = leaf)) +
  facet_wrap(~ latin_name) +  # Panel for each group
  labs(title = "Comparison of OLS and Mixed-Effects Models",
       x = "Predictor Variable",
       y = "Response Variable")

scatterplot + geom_line(colour="red",aes(y=prepop))
  geom_line(colour="dark grey",aes(y=predindi,group=id)) # fitted lines