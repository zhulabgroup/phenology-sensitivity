# adapted from complete_npn.qmd in the main branch

## combine genus data --------
folder_path <- .path$npn

# Get list of RDS files in the folder
rds_files <- list.files(path = folder_path, pattern = "\\.rds$", full.names = TRUE)

# Initialize an empty data frame to store the combined data
combined_data <- data.frame()

# Loop through each RDS file and combine the data
for (i in seq_along(rds_files)) {
  # Read the RDS file and append to the combined_data
  data <- read_rds(rds_files[i]) %>%
    mutate(taxa = sub(".*\\/([^/]+)\\.rds$", "\\1", rds_files[i]))
  combined_data <- rbind(combined_data, data)
}

## clean the combined data --------
complete_npn_data <- combined_data %>%
  filter(pheno_class_id == 7) %>% # 22979  # flower
  filter(observed_status_conflict_flag == "-9999") %>% # 20489   # Remove rows with status conflicts
  group_by(individual_id, first_yes_year) %>%
  arrange(first_yes_doy) %>% # Keep only the earlist first yes doy for each individual in each year
  slice(1) %>% # 16086
  ungroup() %>%
  filter(numdays_since_prior_no <= 7 & numdays_since_prior_no > 0) %>% # 8563  #only keep observation with a no observation within 7 days in advance
  dplyr::select(individual_id, first_yes_year, first_yes_doy, species, genus, dataset_id, pheno_class_id, longitude, latitude, taxa) %>%
  rename(lat = latitude, lon = longitude, year = first_yes_year, doy = first_yes_doy) %>%
  mutate(species = paste(genus, species, sep = " ")) 

write.csv(complete_npn_data, .path$npn_flower)
