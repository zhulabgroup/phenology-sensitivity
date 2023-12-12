atlas_path <-  /nfs/turbo/seas-zhukai/phenology/USTreeAtlas/
atlas_list <-  read_csv("/nfs/turbo/seas-zhukai/phenology/USTreeAtlas/Little_datatable.csv") %>% 
  rename(latin_name = `Latin Name`)

species_code <- rnpn::npn_species() 

latin_name_list <- species_code %>% 
  dplyr::select('genus', 'species', 'species_id','common_name') %>% 
  mutate(latin_name = paste0(genus, " ", species))

unique_species <- data.frame(species_id = unique(maple$species_id))
maple_list <- left_join(unique_species, latin_name_list, by = "species_id") %>%
  left_join(atlas_list, by = c("latin_name" = "latin_name"))



maple <- read_rds("data/different_species/maple.rds")

us_map <- map_data("state")

data_filtered <- maple %>%
  filter(common_name == "sugar maple") %>%
  distinct(longitude, latitude) 


ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_point(data = data_filtered, aes(x = longitude, y = latitude), size = 2, color = "red", alpha = 0.1) +
  coord_fixed(ratio = 1.5) +  # Adjust the aspect ratio for a better display of the US
  labs(x = "Longitude", y = "Latitude") +  # Label axes
  theme_minimal() 