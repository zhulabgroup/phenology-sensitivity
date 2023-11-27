
# Load the data and source functions
quercus <- readRDS("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/Quercus_winsprtem.rds") 
source("function_plot_spatial_sensitive.R")
source("function_plot_temporal_sensitive.R")

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
      mutate_at(vars(flower, lag, leaf, winter_avg_temp, spring_avg_temp), 
                ~ . - mean(.)) %>%
      ungroup()
      
      temporal_leaf <- plot_temporal_sensitive(data_filtered, "leaf")
      temporal_flower <- plot_temporal_sensitive(data_filtered, "flower")
      temporal_leaf / temporal_flower
  })
  
  output$spatialPlot <- renderPlot({
    data_filtered <- quercus %>%
      filter(common_name %in% input$species) %>%
      group_by(longitude, latitude) %>%
      summarise(
        across(
          c(leaf, flower, lag, winter_avg_temp, spring_avg_temp),
          list(
            Mean = ~mean(.),
            Min = ~min(.)-mean(.),
            Max = ~max(.)-mean(.)
          ),
          .names = "{.col}_{.fn}"
        )
      ) %>%   
      ungroup() %>% 
      mutate_at(vars(flower_Mean, lag_Mean, leaf_Mean, winter_avg_temp_Mean, spring_avg_temp_Mean), 
                ~ . - mean(.)) 
      
      spatial_leaf <- plot_spatial_sensitive(data_filtered, "leaf")
      spatial_flower <- plot_spatial_sensitive(data_filtered, "flower")
      spatial_leaf / spatial_flower
  })
  
  output$spatial_distribution <- renderPlot({
    us_map <- map_data("state")
    
    data_filtered <- quercus %>%
      filter(common_name %in% input$species) %>%
      distinct(longitude, latitude) 
      
    
    ggplot() +
      geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
      geom_point(data = data_filtered, aes(x = longitude, y = latitude), size = 2, color = "red", alpha = 0.1) +
      coord_fixed(ratio = 1.5) +  # Adjust the aspect ratio for a better display of the US
      labs(x = "Longitude", y = "Latitude") +  # Label axes
      theme_minimal() 
  })
  
}

# Run the application
