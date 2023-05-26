library(tidyverse)
source("scripts/function_circular_regression.R")
# Folder path
folder_path <- "/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/" 

# Get list of RDS files in the folder
rds_files <- list.files(path = folder_path, pattern = "\\.rds$", full.names = TRUE)

# Loop through each RDS file

for (i in seq_along(rds_files) ) {
  # Read the RDS file
data <- read_rds(rds_files[i])
  
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
    next
  }
  
  models <- selected_data %>% group_by(species_id) %>%
    summarise(model = list(mycircular(first_yes_doy.x, first_yes_doy.y))) %>% 
    unnest_wider(model) %>% 
    unnest_longer(c("x", "y", "y_fit"))
  
  site_gg <- vector(mode = "list")
  site_gg <- models %>% 
    group_by(species_id)%>% 
    group_split() %>%
    map(~create_plot(.))


  
  pdf(str_c(rds_files[i], ".pdf"), width = 8, height = 8 * .618)
  print(site_gg)
  dev.off()
  
}


create_plot <- function(data) {
  ggplot(data) +
  geom_point(aes(x = x, y = y), col = "blue", pch = 16) +
  geom_point(aes(x = x, y = y_fit), col = "red", pch = 16) +
    ggtitle(as.character(sprintf("%.3f",unique(data$coefficient)))) +
    scale_y_continuous(labels = function(y) format(as.Date("2000-01-01") + y - 1, "%b %d"))+
    scale_x_continuous(labels = function(y) format(as.Date("2000-01-01") + y - 1, "%b %d"))

}



