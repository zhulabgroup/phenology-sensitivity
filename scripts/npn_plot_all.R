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
    select(individual_id, first_yes_year, first_yes_doy, species_id, dataset_id, pheno_class_id)
  
  joined_data <- data_qc %>%
    filter(pheno_class_id == 1) %>%
    inner_join(data_qc %>%
                 filter(pheno_class_id == 7),
               by = c("individual_id", "first_yes_year", "species_id", "dataset_id"))
  
  site_gg[[i]] <- ggplot(joined_data) +
    geom_point(aes(x = first_yes_doy.x, y = first_yes_doy.y, color = as.factor(species_id) ), alpha = 0.2) +
    geom_smooth(method = "lm", aes(x = first_yes_doy.x, y = first_yes_doy.y, color = as.factor(species_id)), se = FALSE) +
    xlab("Leafing Day") +
    ylab("Flowering Day") +
    ggtitle(as.character(data$genus[1]) ) +
    geom_abline(intercept = 0, slope = 1)

}

pdf("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/taxa.pdf", width = 8, height = 8 * .618)
print(site_gg)
dev.off()
