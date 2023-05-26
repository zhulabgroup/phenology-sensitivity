library(tidyverse)
source("scripts/function_circular_regression.R")
# Folder path
folder_path <- "/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/" 

# Get list of RDS files in the folder
rds_files <- list.files(path = folder_path, pattern = "\\.rds$", full.names = TRUE)

# Loop through each RDS file
read_and_process_data <- function(file) {

  data <- read_rds(file)
  
  data_qc <- data %>%   filter(observed_status_conflict_flag == "-9999") %>% # Removing Status Conflicts
    group_by(individual_id, first_yes_year, pheno_class_id) %>%
    arrange(first_yes_doy) %>%
    slice(1)%>%
    mutate(numdays_since_prior_no = na_if(numdays_since_prior_no, "-9999")) %>% # set the -9999 values to NA
    filter(numdays_since_prior_no < 20) %>% # Filtering Data by Prior No
    dplyr::select(individual_id, first_yes_year, first_yes_doy, species_id, dataset_id, pheno_class_id)
  
  joined_data <- data_qc %>%
    filter(pheno_class_id == 1) %>%
    inner_join(data_qc %>%
                 filter(pheno_class_id == 7),
               by = c("individual_id", "first_yes_year", "species_id", "dataset_id"))
  
  selected_data <- joined_data %>%
    group_by(species_id) %>%
    filter(n() > 30) %>% 
    ungroup() 

  
  if (nrow(selected_data) == 0) {
    return(NULL)
  }
  selected_data
}

plot_species <- function(data) {
  file <- names(data)
  species_list <- undata$species_id
  temp <- data[data$species_id==species_id,]
  plotdata <- mycircular(temp$first_yes_doy.x, temp$first_yes_doy.y)
  
  ggplot() +
    geom_point(aes(x = plotdata$x, y = plotdata$y), col = "blue", pch = 16) +
    geom_point(aes(x = plotdata$x, y = plotdata$y_fit), col = "red", pch = 16) +
    ggtitle(tools::file_path_sans_ext(basename(file))) 
}


processed_data <- rds_files %>% 
  map(read_and_process_data) %>%
  set_names(rds_files) %>%
  discard(is.null) 

test <- function(data){print(names(data))}

plot_data <- processed_data %>% 
  map(~ test) %>%
  unlist(recursive = FALSE)
  
  

model <- data %>%
  group_by(species_id) %>%
  summarise(model = list(mycircular(first_yes_doy.x, first_yes_doy.y))) 

model_un <- model %>% unnest_wider(model) %>% unnest_longer(c("x", "y", "y_fit"))

model_un %>% group_by(species_id) %>% map(~plot_circular)

plot.circular <- function(data){
  ccfigure <- %>% ggplot() +
    geom_point(aes(x, y), col = "blue", pch = 16) +
    geom_point(aes(x, y_fit), col = "red", pch = 16)
  ccfigure
}
model_un  +
  facet_wrap(~species_id) +
  ggtitle(tools::file_path_sans_ext(basename(file))) 



pdf("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/taxa_cc_acer.pdf", width = 8, height = 8 * .618)
print(site_gg)
dev.off()
