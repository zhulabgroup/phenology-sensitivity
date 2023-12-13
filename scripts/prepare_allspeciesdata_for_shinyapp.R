if (!file.exists("data/different_species/climate_list.rds")) {
 source("scripts/download_daymet_allspecies.R") 
}

species_code <- rnpn::npn_species() %>% 
  dplyr::select('genus', 'species','functional_type', 'common_name', 'species_id') %>% 
  mutate(latin_name = paste0(genus, " ", species))

climate_list <- readRDS("data/different_species/climate_list.rds")
# Folder path
folder_path <- "/nfs/turbo/seas-zhukai/phenology/NPN/individual_phenometrics/leaf_flower/" 

# Get list of RDS files in the folder
rds_files <- list.files(path = folder_path, pattern = "\\.rds$", full.names = TRUE)


for (i in seq_along(rds_files) ) {
  maple <- read_rds(rds_files[i])

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
  
  leaf_code <- maple_m %>% 
    filter(pheno_class_id < 4) %>% 
    group_by(pheno_class_id) %>% 
    summarise(count = n()) %>% 
    arrange(desc(count)) %>%
    slice(1) %>%
    pull(pheno_class_id)
  
  flower_code <- maple_m %>% 
    filter(pheno_class_id > 4) %>% 
    group_by(pheno_class_id) %>% 
    summarise(count = n()) %>% 
    arrange(desc(count)) %>%
    slice(1) %>%
    pull(pheno_class_id)
  
  joined_data <- maple_m %>%
    filter(pheno_class_id == leaf_code) %>%
    inner_join(maple_m %>%
                 filter(pheno_class_id == flower_code),
               by = c("individual_id", "year", "species_id", "dataset_id", "longitude", "latitude")) %>% 
    group_by(species_id) %>% 
    filter(n()>30) %>% 
    ungroup() %>% #2607
    rename(leaf = first_yes_doy.x, flower = first_yes_doy.y)
  
  # join data
  data_shiny <- joined_data %>% 
    filter(year<2023) %>% 
    left_join(climate_list, by = c("latitude", "longitude", "year")) %>% 
    unique() %>% 
    left_join(species_code, by = "species_id") %>% 
    filter(complete.cases(.))
  
  original_file_name <- tools::file_path_sans_ext(basename(rds_files[i]))

  # Create the new file name
  new_file_name <- str_c("data/different_species/", original_file_name, ".rds")
  
  # Write the data_shiny object to the new file

  if (nrow(data_shiny) > 0) {
    writeRDS(data_shiny, new_file_name)
  } 
  
}

