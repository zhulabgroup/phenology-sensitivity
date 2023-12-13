
maple <- read_rds("data/different_species/Acer.rds")


atlas_list <-  read_csv("/nfs/turbo/seas-zhukai/phenology/USTreeAtlas/Little_datatable.csv") %>% 
  rename(latin_name = `Latin Name`)

species_code <- rnpn::npn_species() 

latin_name_list <- species_code %>% 
  dplyr::select('genus', 'species', 'species_id','common_name') %>% 
  mutate(latin_name = paste0(genus, " ", species))

unique_species <- data.frame(species_id = unique(maple$species_id))
maple_list <- left_join(unique_species, latin_name_list, by = "species_id") %>%
  left_join(atlas_list, by = c("latin_name" = "latin_name"))

shp_name <- maple_list %>% 
  filter(common_name == "sugar maple") %>% 
  pull(`SHP/*`)


data_filtered <- maple %>%
  filter(common_name == "sugar maple") %>%
  distinct(longitude, latitude) 

library(sf)
shp_path <- paste0("/nfs/turbo/seas-zhukai/phenology/USTreeAtlas/shp/", shp_name, "/",shp_name,".shp")
shp_data <- st_read(shp_path)

us_map <- map_data("state")

base_map <- ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") +
  theme_minimal()


combined_map <- base_map +
  geom_sf(data = shp_data, fill = "blue", alpha = 0.1, inherit.aes = FALSE)

final_plot <- combined_map +
  geom_point(data = data_filtered, aes(x = longitude, y = latitude), 
             size = 2, color = "red", alpha = 0.1) +
  coord_sf() +  # Use coord_sf for accurate geographical plotting
  labs(x = "Longitude", y = "Latitude")

final_plot


