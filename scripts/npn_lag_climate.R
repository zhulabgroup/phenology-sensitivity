library(tidyverse)
library(circular)

# Folder path
folder_path <- "/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/climate/" 

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
    dplyr::select(individual_id, first_yes_year, first_yes_doy, species_id, dataset_id, pheno_class_id, gdd)
  
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
    summarise(model = list(mycircular(first_yes_doy.x, first_yes_doy.y)), gdd_x = list(gdd.x), gdd_y = list(gdd.y) )%>%
    filter(map_lgl(model, ~ .x$coefficient$p.value < 0.05)) %>%
    unnest_wider(model) %>% 
    unnest_longer(c("x", "y", "y_fit","gdd_x","gdd_y")) %>% 
    filter(gdd_x>0) %>% 
    filter(gdd_y>0)
  
  lag_figure <- models %>% 
    mutate(lag = y - x,
           lag = ifelse(lag < -182, lag + 365, ifelse(lag > 182, lag - 365, lag))) %>%
    ggplot(aes(x = gdd_x, y = lag, alpha = 0.1)) +
    geom_point()+
    facet_wrap(~species_id) +
  ggtitle(gsub("\\.rds$", "", basename(rds_files[i])))
  

  site_gg <- c(site_gg, list(lag_figure))
  
}

pdf("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/climate/lag_climate_x.pdf" , width = 8, height = 8 * .618)
print(site_gg)
dev.off()