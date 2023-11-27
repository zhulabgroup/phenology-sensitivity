
#test
quercus_leaf <- read_rds("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/neon/Quercus_leaf_winsprtem.rds")
quercus_flower <- read_rds("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/neon/Quercus_flower_winsprtem.rds")

test <- quercus_leaf %>% 
  group_by(common_name) %>% 
  summarise(count = n()) %>% 
  filter(count>10)

source("function_plot_spatial_sensitive.R")
source("function_plot_temporal_sensitive.R")

# Define UI
ui <- fluidPage(
  titlePanel("Phenology Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("species", "Select Species:",
                  choices = test$common_name)
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("temporalPlot")),
        column(6, plotOutput("spatialPlot"))
      ),
      fluidRow(
        column(12, plotOutput("spatial_distribution_leaf")),
        column(12, plotOutput("spatial_distribution_flower"))
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  output$temporalPlot <- renderPlot({
    data_filtered_leaf <-  quercus_leaf %>%
      filter(common_name %in% input$species) %>%
      group_by(individual_id) %>%
      mutate_at(vars(first_yes_doy, spring_avg_temp), 
                ~ . - mean(.)) %>%
      ungroup() %>% 
      rename(leaf = first_yes_doy)
    
    data_filtered_flower <-  quercus_flower %>%
      filter(common_name %in% input$species) %>%
      group_by(individual_id) %>%
      mutate_at(vars(first_yes_doy, spring_avg_temp), 
                ~ . - mean(.)) %>%
      ungroup() %>% 
      rename(flower = first_yes_doy)
    
    temporal_leaf <- plot_temporal_sensitive(data_filtered_leaf, "leaf")
    temporal_flower <- plot_temporal_sensitive(data_filtered_flower, "flower")
    temporal_leaf / temporal_flower
  })
  
  output$spatialPlot <- renderPlot({
    data_filtered_leaf <- quercus_leaf %>%
      filter(common_name %in% input$species) %>% 
      rename(leaf = first_yes_doy) %>%
      group_by(longitude, latitude) %>%
      summarise(
        across(
          c(leaf, spring_avg_temp),
          list(
            Mean = ~mean(.),
            Min = ~min(.)-mean(.),
            Max = ~max(.)-mean(.)
          ),
          .names = "{.col}_{.fn}"
        )
      ) %>%   
      ungroup() %>% 
      mutate_at(vars(leaf_Mean, spring_avg_temp_Mean), 
                ~ . - mean(.)) 
    
      data_filtered_flower <- quercus_flower %>%
        filter(common_name %in% input$species) %>% 
        rename(flower = first_yes_doy) %>%
        group_by(longitude, latitude) %>%
        summarise(
          across(
            c(flower, spring_avg_temp),
            list(
              Mean = ~mean(.),
              Min = ~min(.)-mean(.),
              Max = ~max(.)-mean(.)
            ),
            .names = "{.col}_{.fn}"
          )
        ) %>%   
        ungroup() %>% 
        mutate_at(vars(flower_Mean, spring_avg_temp_Mean), 
                  ~ . - mean(.)) 
    
    spatial_leaf <- plot_spatial_sensitive(data_filtered_leaf, "leaf")
    spatial_flower <- plot_spatial_sensitive(data_filtered_flower, "flower")
    spatial_leaf / spatial_flower
  })
  
  output$spatial_distribution_leaf <- renderPlot({
    us_map <- map_data("state")
    
    data_filtered_leaf <- quercus_leaf %>%
      filter(common_name %in% input$species) %>%
      distinct(longitude, latitude) 
    
    data_filtered_flower <- quercus_flower %>%
      filter(common_name %in% input$species) %>%
      distinct(longitude, latitude) 
    
    ggplot() +
      geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
      geom_point(data = data_filtered_leaf, aes(x = longitude, y = latitude), size = 2, color = "green", alpha = 0.2) +
      geom_point(data = data_filtered_flower, aes(x = longitude, y = latitude), size = 2, color = "yellow", alpha = 0.2) +
      coord_fixed(ratio = 1.5) +  # Adjust the aspect ratio for a better display of the US
      labs(x = "Longitude", y = "Latitude") +  # Label axes
      theme_minimal() 
  })
}

# Run the application
