if (!file.exists("data/different_species/maple.rds")) {

maple <- read_rds("/nfs/turbo/seas-zhukai/phenology/NPN/individual_phenometrics/leaf_flower/Acer.rds") 

maple_m <- maple %>% 
  filter(observed_status_conflict_flag == "-9999") %>% # Removing Status Conflicts (no conflict for neon)
  group_by(individual_id, first_yes_year, pheno_class_id)  %>%
  filter(n() == 1) %>%  # Keep only groups with one observation 41580, multiple_firsty == 0 can still keep some with 2 observations
  ungroup()  %>%
  mutate(numdays_since_prior_no = as.numeric(numdays_since_prior_no)) %>%
  filter(numdays_since_prior_no > 0) %>% # Filtering Data by Prior No 29264
  filter(numdays_since_prior_no < 30) %>% # Filtering Data by Prior No 27377
  dplyr::select(individual_id, first_yes_year, first_yes_doy, species_id, dataset_id, pheno_class_id, longitude, latitude) %>% 
  ungroup() %>% 
  rename(year = first_yes_year)

ggplot(maple_m, aes(x = as.factor(pheno_class_id))) +
  geom_bar() +
  labs(x = "Pheno Class ID", y = "Count") +
  ggtitle("Boxplot of Counts by Pheno Class ID") 

# 3 has the most observation and can show leaf
# 7 has the most observation and can show flower

joined_data <- maple_m %>%
  filter(pheno_class_id == 3) %>%
  inner_join(maple_m %>%
               filter(pheno_class_id == 7),
             by = c("individual_id", "year", "species_id", "dataset_id", "longitude", "latitude")) %>% 
  group_by(species_id) %>% 
  filter(n()>30) %>% 
  ungroup() %>% #2607
rename(leaf = first_yes_doy.x, flower = first_yes_doy.y)
  

# prepare tem for all locations for both leaf and flower
source("scripts/function_download_daymet_seasonal_tem.R")

quercus_tem <- joined_data %>% 
  dplyr::select("latitude", "longitude", "year") %>% 
  unique() %>% 
  filter(year<2023 & longitude<0) %>% # 2 deleted for wrong longitude, 205 for 2023 observation
  get_winter_spring_temperatures()

species_code <- rnpn::npn_species() %>% 
  dplyr::select('functional_type', 'common_name', 'species_id') 

# join data
data_shiny <- joined_data %>% 
  filter(year<2023 & longitude<0) %>% 
  left_join(quercus_tem, by = c("latitude", "longitude", "year")) %>% 
  unique() %>% 
  left_join(species_code, by = "species_id")

write_rds(data_shiny, "data/different_species/maple.rds")

} 

quercus <- read_rds("data/different_species/maple.rds")


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

# Run the applica



