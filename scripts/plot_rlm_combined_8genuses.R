# combined figure for rlm for npn data:
source("scripts/function_get_clean_npn_first.R")
library(MASS)

# Folder path
folder_path <- "/nfs/turbo/seas-zhukai/phenology/NPN/individual_phenometrics/leaf_flower/" 

# Get list of RDS files in the folder
rds_files <- list.files(path = folder_path, pattern = "\\.rds$", full.names = TRUE)

# Loop through each RDS file
site_gg <- list()
genus_stat <- c()
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
  


  # Group by species and calculate robust linear regression for each group
  grouped_results <- taxadata %>%
    group_by(species_id) %>%
    summarize(
      n = n(),
      slope = broom::tidy(rlm(flower ~ leaf))$estimate[2], # Extract slope (coefficient for "leaf")
      intercept = broom::tidy(rlm(flower ~ leaf))$estimate[1] # Extract intercept
    )
  
  # Store the genus-level statistics and coefficients
  genus_stat[[i]] <- list(
    genus_name = taxa_name,
    genus_n = nrow(taxadata),
    coefficients = broom::tidy(rlm(flower ~ leaf, data = taxadata)),
    species_results = grouped_results
  )
  
}
}

combined_plot <- site_gg %>% 
  discard(is.null) %>% 
  patchwork::wrap_plots(nrow = 2)

