# check the statistics

rmse <- read_csv("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/lag_RMSE_Smooth_Quercus_leafflowerpollen.csv")

t.test(rmse$leaf_lag_rmse, rmse$flower_rmse)

t.test(rmse$leaf_lag_rmse, rmse$flower_rmse, paired = TRUE)


total_compare <- data.frame(
  Type = rep(c("Leaf", "Flower"), each = length(rmse$leaf_lag_rmse)),
  RMSE = c(rmse$leaf_lag_rmse, rmse$flower_rmse)
)

total_counts <- total_compare %>% 
  group_by(Type) %>% 
  summarise(n = sum(!is.na(RMSE)))

p1 <- ggplot(total_compare, aes(x = Type, y = RMSE, fill = Type)) +
  geom_boxplot() +
  geom_text(data = total_counts, aes(label = n, y = Inf), vjust = 7) +  # adjust y and vjust to position the labels
  labs(x = " ", y = "Normalized RMSE")  +
  theme_bw() +
  theme(legend.position = "none")

# Your data transformation
rmse_diff <- rmse %>%
  mutate(diff = flower_rmse-leaf_rmse)

# Calculate the number of non-NA observations
diff_counts <- rmse_diff %>%
  summarise(n = sum(!is.na(diff)))

p2 <- rmse_diff %>%
  ggplot(aes(x = "", y = diff)) +
  geom_boxplot() +
  geom_text(data = diff_counts, aes(label = n, y = Inf), vjust = 3) +  # adjust y and vjust to position the labels
  labs(y = "Difference (Flower RMSE - Leaf RMSE)", x = "") +
  theme_bw()

# get npn data
npn_wind_from_phenoclass <- function(phenoclass, taxa) {
  # Load the phenophase data
  npn_phenophases <- rnpn::npn_phenophases()
  
  # Extract the phenophase_id based on the input phenoclass
  id <- npn_phenophases %>%
    filter(pheno_class_id == phenoclass) %>%
    select(phenophase_id)
  
  # Read the wind data for the given taxa
  npn_wind <- read_rds(paste0("/nfs/turbo/seas-zhukai/phenology/NPN/wind_poll_taxa/", taxa, ".rds")) %>%
    filter(phenophase_id %in% id$phenophase_id) %>%
    select(latitude, longitude, individual_id, observation_date, phenophase_status, species_id) %>%
    mutate(observation_date = as.Date(observation_date),
           year = year(observation_date),
           doy = yday(observation_date)) %>% 
    filter(phenophase_status > -1) %>% 
    unique() %>%
    group_by(year, doy, individual_id) %>%
    filter(n() == 1) %>%
    ungroup()
  
  return(npn_wind)
}

# Usage example:
# Replace 'phenoclass_value' and 'taxa_value' with your desired values
leaf <- npn_wind_from_phenoclass(phenoclass = 1, taxa = "Quercus")
flower <- npn_wind_from_phenoclass(phenoclass = 7, taxa = "Quercus")

# Calculate grid cells and the number of observations in each cell
leaf_grid <- flower %>%
  mutate(lat_grid = floor(latitude * 10) / 10,
         lon_grid = floor(longitude * 10) / 10) %>%
  group_by(lat_grid, lon_grid) %>%
  summarise(observations = n())

# Filter map data to include only the continental 
us_map <- map_data("world") %>%
  filter(region %in% c("USA", "US48"))

# Create the plot
ggplot() +
  geom_tile(data = leaf_grid, aes(x = lon_grid, y = lat_grid, fill = observations)) +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
  coord_fixed(1.3) +  # Adjust the aspect ratio to avoid distortion
  scale_fill_viridis_c(name = "Number of Observations", label = scales::comma) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Spatial Coverage of Observations in 1-degree x 1-degree Grids") +
  theme_minimal()
