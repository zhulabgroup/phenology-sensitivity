
# Load the data and source functions
quercus <- readRDS("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/Quercus_winsprtem.rds") 
source("function_plot_spatial_sensitive.R")
source("function_plot_temporal_sensitive.R")

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
      group_by(individual_id, longitude, latitude, common_name) %>%
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
  
}

# Run the application
