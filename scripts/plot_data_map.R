# Create a map of the continental US

us_map <- map_data("state")


quercus <- read_rds("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/Quercus_winsprtem.rds")

individual_aggre <- quercus %>%
  filter(common_name %in% c("northern red oak")) %>% 
  group_by(longitude, latitude) %>%
  summarise(across(c(leaf, flower, lag, winter_avg_temp, spring_avg_temp), mean, .names = "{.col}")) %>% 
  ungroup() 


# Create a scatterplot of data points on top of the map
data_map <- ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_point(data = individual_aggre, aes(x = longitude, y = latitude), size = 2, color = "red", alpha = 0.1) +
  coord_fixed(ratio = 1.5) +  # Adjust the aspect ratio for a better display of the US
  labs(x = "Longitude", y = "Latitude") +  # Label axes
  theme_minimal()  # Optional: Apply a minimal theme
