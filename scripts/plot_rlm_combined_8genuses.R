# combined figure for rlm for npn data:
source("scripts/function_get_clean_npn_first.R")
library(MASS)

species_code <- rnpn::npn_species() %>% 
  dplyr::select('functional_type', 'common_name', 'species_id')

# Folder path
folder_path <- "/nfs/turbo/seas-zhukai/phenology/NPN/individual_phenometrics/leaf_flower/" 

# Get list of RDS files in the folder
rds_files <- list.files(path = folder_path, pattern = "\\.rds$", full.names = TRUE)

# Loop through each RDS file
site_gg <- list()
species_stat <- tibble()

for (i in seq_along(rds_files) ) {
  # Read the RDS file
  taxadata <- read_rds(rds_files[i]) %>% 
    get_clean_npn_first() %>% 
    rename(leaf = first_yes_doy.x, flower = first_yes_doy.y, year = first_yes_year) %>% 
    dplyr::select(species_id,leaf,flower,year)
  
  if (nrow(taxadata)!=0) {
    taxa_name <- stringr::str_extract(rds_files[i], "(?<=//)(.*?)(?=\\.rds)")
    
  site_gg[[i]] <- ggplot(taxadata) +
    geom_point(aes(x = leaf, y = flower, color = as.factor(species_id) ), alpha = 0.1) +
    geom_smooth(method = "rlm", aes(x = leaf, y = flower, color = as.factor(species_id)), se = FALSE) +
    geom_smooth(method = "rlm", aes(x = leaf, y = flower), color = "#000000",linewidth = 2, se = FALSE) +
    xlab("Leafing Day") +
    ylab("Flowering Day") +
    ggtitle(taxa_name) +
    geom_abline(intercept = 0, slope = 1)+
    guides(color = FALSE)
  

  genus_result <- taxadata %>% 
    summarize(
      n = n(),
      slope = broom::tidy(rlm(flower ~ leaf))$estimate[2], # Extract slope (coefficient for "leaf")
      intercept = broom::tidy(rlm(flower ~ leaf))$estimate[1] # Extract intercept
    ) %>% 
    mutate(scientific_name = taxa_name)
    
  # Group by species and calculate robust linear regression for each group
  grouped_results <- taxadata %>%
    group_by(species_id) %>%
    summarize(
      n = n(),
      slope = broom::tidy(rlm(flower ~ leaf))$estimate[2], # Extract slope (coefficient for "leaf")
      intercept = broom::tidy(rlm(flower ~ leaf))$estimate[1] # Extract intercept
    ) %>% 
    left_join(species_code,by = "species_id") %>% 
    dplyr::select(-species_id,-functional_type) 
  
  species_stat <- bind_rows(species_stat, genus_result, grouped_results) 
  
}
}



combined_plot <- site_gg %>% 
  discard(is.null) %>% 
  patchwork::wrap_plots(nrow = 2)

