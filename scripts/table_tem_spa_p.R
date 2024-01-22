# Read the RDS file
maple <- read_rds("/Volumes/seas-zhukai/phenology/phenology_leaf_flower_lag/different_species/Acer.rds")

# Corrected analysis
filtered_data_individual <- maple %>%
  group_by(individual_id, latin_name) %>%
  summarise(
    mean_spring_avg_temp = mean(spring_avg_temp, na.rm = TRUE),  # Calculate mean, removing NA values
    n = n(),  # Count number of observations in each group
    slope = {  # Calculate the slope of the linear model within each group
      model <- lm(leaf ~ spring_avg_temp, data = cur_data())  # Using cur_data()
      coef(model)[["spring_avg_temp"]]  # Extract the slope coefficient
    },
    .groups = 'drop'  # Drop the grouping after summarising
  ) %>%
  filter(n > 2)  # Filter groups with more than 2 observations

filtered_data_species <- maple %>%
  filter(individual_id %in% filtered_data_individual$individual_id) %>% 
  group_by(latin_name, common_name) %>%
  summarise(
    mean_spring_avg_temp = mean(spring_avg_temp, na.rm = TRUE),  # Calculate mean, removing NA values
    n = n(),  # Count number of observations in each group
    slope = {  # Calculate the slope of the linear model within each group
      model <- lm(leaf ~ spring_avg_temp, data = cur_data())  # Using cur_data()
      coef(model)[["spring_avg_temp"]]  # Extract the slope coefficient
    },
    .groups = 'drop'  # Drop the grouping after summarising
  ) 


# Adding new columns to filtered_data_species
filtered_data_species <- filtered_data_species %>%
  mutate(
    smaller_n = map_dbl(latin_name, function(species_name) {
      species_slope <- slope[latin_name == species_name]
      sum(filtered_data_individual$slope[filtered_data_individual$latin_name == species_name] < species_slope)
    }),
    total_individuals = map_dbl(latin_name, function(species_name) {
      sum(filtered_data_individual$latin_name == species_name)
    })
  )

# Extracting slopes for Acer saccharum from filtered_data_individual
slopes_individual <- filtered_data_individual %>%
  filter(latin_name == "Acer saccharum") %>%
  pull(slope)

# Extracting slope for Acer saccharum from filtered_data_species
slope_species <- filtered_data_species %>%
  filter(latin_name == "Acer saccharum") %>%
  pull(slope)

# Creating a density plot
ggplot(mapping = aes(x = slopes_individual)) + 
  geom_density() +  # Density plot for individual slopes
  geom_vline(aes(xintercept = mean(slope_species), color = "Spatial Slope"), linetype = "dashed") +
  geom_vline(aes(xintercept = mean(slopes_individual), color = "Average temporal Slope"), linetype = "dashed") +
  labs(title = "Density Plot for Sugar Maple Slopes, leaf",
       x = "Slope",
       y = "Density") +
  theme_minimal() +
  theme(legend.title = element_blank())


