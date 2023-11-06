source("scripts/function_get_clean_npn_first.R")
library(MASS)

species_code <- rnpn::npn_species() %>% 
  dplyr::select('functional_type', 'common_name', 'species_id')

# Folder path
folder_path <- "/nfs/turbo/seas-zhukai/phenology/NPN/individual_phenometrics/leaf_flower/" 

# Get list of RDS files in the folder
rds_files <- list.files(path = folder_path, pattern = "\\.rds$", full.names = TRUE)[c(1, 10)]

# Loop through each RDS file
site_gg <- list()

for (i in seq_along(rds_files) ) {
  # Read the RDS file
  taxadata <- read_rds(rds_files[i]) %>% 
    get_clean_npn_first() %>% 
    rename(leaf = first_yes_doy.x, flower = first_yes_doy.y, year = first_yes_year) %>% 
    dplyr::select(species_id,leaf,flower,year,latitude) %>% 
    left_join(species_code,by = "species_id")
  
  # Calculate the count of observations per common_name
  count_data <- taxadata %>%
    group_by(common_name) %>%
    summarize(count = n())
  
  # Reorder the levels of common_name based on the count in descending order
  taxadata$common_name <- factor(taxadata$common_name, levels = count_data$common_name[order(-count_data$count)])
  
  taxa_name <- stringr::str_extract(rds_files[i], "(?<=//)(.*?)(?=\\.rds)")
  
  
    site_gg[[i]] <- ggplot(taxadata) +
      geom_point(aes(x = leaf, y = flower),  alpha = 0.1) +
      geom_smooth(method = "rlm", aes(x = leaf, y = flower), color = "black", se = FALSE) +
      xlab("Leafing Day") +
      ylab("Flowering Day") +
      ggtitle(taxa_name) +
      geom_abline(intercept = 0, slope = 1,color = "red")+
      facet_wrap(~common_name, ncol = 8) +
      theme_bw() +
      theme(strip.text = element_text(face = "italic"))
}


combined_plot <- site_gg[[1]]+site_gg[[2]] + 
  patchwork::plot_layout(design = 
                           "A
                            B
                            B"
  )
