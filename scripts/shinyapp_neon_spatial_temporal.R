library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(patchwork)

# Load the data and source functions
quercus_leaf <- readRDS("/Volumes/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/Quercus_winsprtem.rds") 
quercus_flower <- readRDS("/Volumes/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/Quercus_winsprtem.rds") 

#test
quercus_leaf <- joined_data_leaf
quercus_flower <- joined_data_flower

source("scripts/function_plot_spatial_sensitive.R")
source("scripts/function_plot_temporal_sensitive.R")

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
      group_by(individual_id, longitude, latitude, common_name) %>%
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
    
      data_filtered_flower <- quercus_leaf %>%
        filter(common_name %in% input$species) %>% 
        rename(flower = first_yes_doy) %>%
        group_by(individual_id, longitude, latitude, common_name) %>%
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
  
}

# Run the application
shinyApp(ui = ui, server = server)
