# combined figure for rlm for npn data:

library(MASS)

# Folder path
folder_path <- "/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/" 

# Get list of RDS files in the folder
rds_files <- list.files(path = folder_path, pattern = "\\.rds$", full.names = TRUE)[c(1, 3, 5, 7, 8, 9, 10, 11)] 

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
  
  site_gg[[i]] <- ggplot(joined_data) +
    geom_point(aes(x = first_yes_doy.x, y = first_yes_doy.y, color = as.factor(species_id) ), alpha = 0.1) +
    geom_smooth(method = "rlm", aes(x = first_yes_doy.x, y = first_yes_doy.y, color = as.factor(species_id)), se = FALSE) +
    geom_smooth(method = "rlm", aes(x = first_yes_doy.x, y = first_yes_doy.y), color = "#000000",linewidth = 2, se = FALSE) +
    xlab("Leafing Day") +
    ylab("Flowering Day") +
    ggtitle(as.character(data$genus[1]) ) +
    geom_abline(intercept = 0, slope = 1)+
    guides(color = FALSE)
  
}

combined_plot <- patchwork::wrap_plots(site_gg, nrow = 2, ncol = 4)