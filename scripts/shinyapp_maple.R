library(sf)


quercus <- read_rds("data/different_species/Acer.rds")
# Read and rename atlas data
atlas_list <- read_csv("/nfs/turbo/seas-zhukai/phenology/USTreeAtlas/Little_datatable.csv") %>% 
  rename(latin_name = `Latin Name`)

source("scripts/function_plot_spatial_sensitive.R")
source("scripts/function_plot_temporal_sensitive.R")

# Define UI
# Define UI
ui <- fluidPage(
  titlePanel("Phenology Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("species", "Select Species:",
                  choices = unique(quercus$common_name))
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("temporalPlot")),
        column(6, plotOutput("spatialPlot"))
      ),
      fluidRow(
        column(12, plotOutput("spatial_distribution"))
      )
    )
  )
)


# Define Server
server <- function(input, output) {
  
  output$temporalPlot <- renderPlot({
    data_filtered <-  quercus %>%
      filter(common_name %in% input$species) %>%
      group_by(individual_id) %>%
      mutate_at(vars(flower, leaf, winter_avg_temp, spring_avg_temp), 
                ~ . - mean(.)) %>%
      ungroup()
    
    temporal_leaf <- plot_temporal_sensitive(data_filtered, "leaf")
    temporal_flower <- plot_temporal_sensitive(data_filtered, "flower")
    temporal_leaf / temporal_flower
  })
  
  output$spatialPlot <- renderPlot({
    data_filtered <- quercus %>%
      filter(common_name %in% input$species) %>%
      group_by(individual_id) %>%
      summarise(
        across(
          c(leaf, flower, winter_avg_temp, spring_avg_temp),
          list(
            Mean = ~mean(.),
            Min = ~min(.)-mean(.),
            Max = ~max(.)-mean(.)
          ),
          .names = "{.col}_{.fn}"
        )
      ) %>%   
      ungroup() %>% 
      mutate_at(vars(flower_Mean, leaf_Mean, winter_avg_temp_Mean, spring_avg_temp_Mean), 
                ~ . - mean(.)) 
    
    spatial_leaf <- plot_spatial_sensitive(data_filtered, "leaf")
    spatial_flower <- plot_spatial_sensitive(data_filtered, "flower")
    spatial_leaf / spatial_flower
  })
  
  output$spatial_distribution <- renderPlot({
    
    data_filtered <- quercus %>%
      filter(common_name %in% input$species) %>%
      distinct(longitude, latitude) 

    target_latin_name <- quercus %>% 
      select(common_name,latin_name) %>% 
      distinct() %>% 
      filter(common_name %in% input$species) %>% 
      pull(latin_name)
    # Filter for sugar maple and extract shapefile name
    shp_name <- atlas_list %>% 
      filter(latin_name == target_latin_name) %>% 
      pull(`SHP/*`)
    
    # Read shapefile
    shp_path <- paste0("/nfs/turbo/seas-zhukai/phenology/USTreeAtlas/shp/", shp_name, "/", shp_name, ".shp")
    shp_data <- st_read(shp_path)
    
    # Prepare base map
    us_map <- map_data("state")
    base_map <- ggplot() +
      geom_polygon(data = us_map, aes(x = long, y = lat, group = group), 
                   fill = "white", color = "black") +
      theme_minimal()
    
    # Plotting
    # --------
    
    # Combine base map with shapefile data and point data
    final_plot <- base_map +
      geom_sf(data = shp_data, fill = "blue", alpha = 0.1, inherit.aes = FALSE) +
      geom_point(data = data_filtered, aes(x = longitude, y = latitude), 
                 size = 2, color = "red", alpha = 0.1) +
      coord_sf() +  # Use coord_sf for accurate geographical plotting
      labs(x = "Longitude", y = "Latitude")
    
    # Display the final plot
    final_plot
  })
  
}

# Run the applica
shinyApp(ui = ui, server = server)


