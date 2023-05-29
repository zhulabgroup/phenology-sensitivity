library(tidyverse)
library(circular)

# Folder path
folder_path <- "/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/" 

# Get list of RDS files in the folder
rds_files <- list.files(path = folder_path, pattern = "\\.rds$", full.names = TRUE)

# Loop through each RDS file
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
  
  if (nrow(selected_data) == 0) {
    next
  }
  
  models <- selected_data %>% group_by(species_id) %>%
    summarise(model = list(mycircular(first_yes_doy.x, first_yes_doy.y))) %>% 
    filter(map_lgl(model, ~ .x$coefficient$p.value < 0.05)) %>% 
    unnest_wider(model) %>% 
    unnest_longer(c("x", "y", "y_fit")) 
  
  lag_figure <- models %>% 
    mutate(lag = y - x,
           lag = ifelse(lag < -182, lag + 365, ifelse(lag > 182, lag - 365, lag))) %>%
    ggplot(aes(x = as.factor(species_id), y = lag)) +
    geom_boxplot() +
    xlab("Species ID") +
    ylab("Flowering day - leafing day") +
    ggtitle(gsub("\\.rds$", "", basename(rds_files[i])))
  
  site_gg <- c(site_gg, list(lag_figure))
  
}

combined_plot <- wrap_plots(site_gg, nrow = 4, ncol = 2)
