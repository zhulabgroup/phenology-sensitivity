library(tidyverse)
source("scripts/function_circular_regression.R")
# Folder path
folder_path <- "/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/" 

# Get list of RDS files in the folder
rds_files <- list.files(path = folder_path, pattern = "\\.rds$", full.names = TRUE)

# Loop through each RDS file
fit_model <- vector(mode = "list")

site_gg <- vector(mode = "list")
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

  
  if (nrow(selected_species) == 0) {
    break
  }

  
  fit_data <- selected_data %>% 
    split(.$species_id) %>%
    map_df(~mycircular(.$first_yes_doy.x, .$first_yes_doy.y), .id = 'species_id')
  
  fit_model[[i]] <- fit_data
  
  site_gg[[i]] <- ggplot(fit_data) +
    geom_point(aes(x = x, y = y, color = "blue")) +
    geom_point(aes(x = x, y = y_fit, color = "red")) +
    xlab("Leafing Day") +
    ylab("Flowering Day") +
    facet_wrap(~species_id, ncol = 3)

}

pdf("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/taxa_rlm.pdf", width = 8, height = 8 * .618)
print(site_gg)
dev.off()
