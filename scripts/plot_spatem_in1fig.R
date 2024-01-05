
create_subplot <- function(target_species) {
  
  data_filtered <- quercus %>%
    filter(common_name %in% target_species) %>%
    group_by(individual_id) %>% 
    filter(n() > 2)
  
  scatter_plot <- ggplot(data_filtered, aes(x = spring_avg_temp, y = flower, color = as.factor(individual_id))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
    geom_smooth(method = "lm", se = FALSE, color = "black") +  # Overall linear regression line in black
    labs(x = "Spring Average Temperature", y = "Leaf") +  # Axis labels
    ggtitle(paste("Scatter Plot and Linear Regression for", target_species, "Individuals")) +
    theme(legend.position = "none")  # Turn off legend for scatter plot
  interactive_scatter_plot <- ggplotly(scatter_plot)
  
  target_latin_name <- quercus %>% 
    select(common_name, latin_name) %>% 
    distinct() %>% 
    filter(common_name %in% target_species) %>%
    pull(latin_name)
  
  shp_name <- atlas_list %>% 
    filter(latin_name == target_latin_name) %>% 
    pull(`SHP/*`)
  
  if (file.exists(paste0("/nfs/turbo/seas-zhukai/phenology/USTreeAtlas/shp/", shp_name, "/", shp_name, ".shp"))) {
    
    shp_path <- paste0("/nfs/turbo/seas-zhukai/phenology/USTreeAtlas/shp/", shp_name, "/", shp_name, ".shp")
    shp_data <- st_read(shp_path)
    
    us_map <- map_data("state")
    base_map <- ggplot() +
      geom_polygon(data = us_map, aes(x = long, y = lat, group = group), 
                   fill = "white", color = "black") +
      theme_minimal()
    
    color_palette <- scale_color_manual(values = unique(data_filtered$individual_id))
    
    final_plot <- base_map +
      geom_sf(data = shp_data, fill = "blue", alpha = 0.1, inherit.aes = FALSE) +
      geom_point(data = data_filtered, aes(x = longitude, y = latitude, color = as.factor(individual_id)), 
                 size = 2) +
      coord_sf() +  # Use coord_sf for accurate geographical plotting
      labs(x = "Longitude", y = "Latitude")
    
    interactive_final_plot <- ggplotly(final_plot) 
    
    subplot(interactive_final_plot, interactive_scatter_plot, nrows = 2)
  }  else {
    # Shapefile doesn't exist, return only the scatter plot
    return(interactive_scatter_plot)
  }
  
}




library(plotly)
library(sf)

quercus <- read_rds("../data/different_species/Acer.rds")
species_list <-  quercus %>%
  group_by(common_name,individual_id) %>% 
  summarise(count = n()) %>% 
  filter(count>2) %>% 
  group_by(common_name) %>% 
  summarise(count = n())

atlas_list <- read_csv("/nfs/turbo/seas-zhukai/phenology/USTreeAtlas/Little_datatable.csv") %>% 
  rename(latin_name = `Latin Name`)


subplot_figures <- lapply(species_list$common_name, create_subplot)

