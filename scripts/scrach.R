# Initialize an empty data frame to store results
results <- data.frame(species_name = character(),
                      spatial_estimate = numeric(),
                      spatial_lower_ci = numeric(),
                      spatial_upper_ci = numeric(),
                      temporal_estimate = numeric(),
                      temporal_lower_ci = numeric(),
                      temporal_upper_ci = numeric(),
                      stringsAsFactors = FALSE)
for (i in 1:23) {
  species_name <- speciesoi$latin_name[i]
  data <- anomaly_data %>%
    filter(latin_name == species_name) %>%
    dplyr::select(leaf, individual_id, spring_avg_temp) %>%
    rename(springT = spring_avg_temp) %>%
    mutate(group = as.integer(factor(individual_id)))
  
  spatial_model <- lm(leaf ~ springT, data = data)
  spatial <- spatial_model$coefficients["springT"]
  confint_spatial <- confint(spatial_model, level = 0.95)["springT",]
  
  temporal_model <- lmerTest::lmer(leaf ~ springT +  (1 | individual_id), data = data) 
  temporal <- fixef(temporal_model)["springT"]
  confint_temporal <- confint(temporal_model, method = "boot", level = 0.95)["springT",]
  
  # Store results
  results <- rbind(results, data.frame(species_name = species_name,
                                       spatial_estimate = spatial,
                                       spatial_lower_ci = confint_spatial[1],
                                       spatial_upper_ci = confint_spatial[2],
                                       temporal_estimate = temporal,
                                       temporal_lower_ci = confint_temporal[1],
                                       temporal_upper_ci = confint_temporal[2]))
}

library(tidyr)
library(dplyr)

# Reshape the results into a long format
results_long <- results %>%
  gather(key = "model_type", value = "estimate", spatial_estimate, temporal_estimate) %>%
  mutate(model_type = ifelse(model_type == "spatial_estimate", "Spatial", "Temporal")) %>%
  # Adding CI information requires a bit more manipulation; it depends on the structure of your original results data frame
  left_join(results %>%
              dplyr::select(species_name, spatial_lower_ci, spatial_upper_ci, temporal_lower_ci, temporal_upper_ci) %>%
              gather(key = "ci_type", value = "ci_value", spatial_lower_ci:temporal_upper_ci),
            by = "species_name") %>%
  mutate(ci_type = case_when(
    model_type == "Spatial" & grepl("spatial_lower", ci_type) ~ "lower",
    model_type == "Spatial" & grepl("spatial_upper", ci_type) ~ "upper",
    model_type == "Temporal" & grepl("temporal_lower", ci_type) ~ "lower",
    model_type == "Temporal" & grepl("temporal_upper", ci_type) ~ "upper",
    TRUE ~ as.character(ci_type)
  )) %>%
  spread(key = ci_type, value = ci_value)

# Plotting
library(ggplot2)

ggplot(results_long, aes(x = estimate, y = species_name, color = model_type)) +
  geom_point(position = position_dodge(width = 0.25), size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, position = position_dodge(width = 0.25)) +
  # Use species_name as the grouping variable to connect spatial and temporal points within each species
  geom_line(aes(group = species_name), position = position_dodge(width = 0.25)) +
  labs(title = "Spatial and Temporal Sensitivity",
       x = "Sensitivity (dDay/dT)", y = "Species") +
  scale_color_manual(values = c("Spatial" = "blue", "Temporal" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())



data <- anomaly_data %>%
  filter(common_name == "sugar maple") %>%
  # filter(latin_name == species_name) %>%
  dplyr::select(leaf, individual_id, spring_avg_temp) %>%
  rename(springT = spring_avg_temp) %>%
  mutate(group = as.integer(factor(individual_id))) # Remap to continuous integer IDs

spatial_model <- lm(leaf ~ springT, data = data)
spatial <- spatial_model$coefficients[[2]]
confint_spatial <- confint(spatial_model, level = 0.95)
springT_confint_spatial <- confint_spatial["springT",]

temporal_model <- lmerTest::lmer(leaf ~ springT +  (1 | individual_id), data = data) 
temporal <- temporal_model@beta[2] 
confint_temporal <- confint(temporal_model, method = "boot", level = 0.95)
springT_confint_temporal <- confint_temporal["springT",]

