# combined figure for rlm for npn data:
source("scripts/function_get_clean_npn_first.R")
library(MASS)

# Folder path
folder_path <- "/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/" 

# Get list of RDS files in the folder
rds_files <- list.files(path = folder_path, pattern = "\\.rds$", full.names = TRUE)[c(1, 3, 5, 7, 9, 10)] 


# Loop through each RDS file
site_gg <- vector(mode = "list")
for (i in seq_along(rds_files) ) {
  # Read the RDS file
  taxadata <- read_rds(rds_files[i]) %>% 
    get_clean_npn_first()
  
  pattern <- "//(.*?)\\.rds"
  
 
  site_gg[[i]] <- ggplot(taxadata) +
    geom_point(aes(x = first_yes_doy.x, y = first_yes_doy.y, color = as.factor(species_id) ), alpha = 0.1) +
    geom_smooth(method = "rlm", aes(x = first_yes_doy.x, y = first_yes_doy.y, color = as.factor(species_id)), se = FALSE) +
    geom_smooth(method = "rlm", aes(x = first_yes_doy.x, y = first_yes_doy.y), color = "#000000",linewidth = 2, se = FALSE) +
    xlab("Leafing Day") +
    ylab("Flowering Day") +
    ggtitle(stringr::str_extract(rds_files[i], "(?<=//)(.*?)(?=\\.rds)")) +
    geom_abline(intercept = 0, slope = 1)+
    guides(color = FALSE)
  
}

combined_plot <- patchwork::wrap_plots(site_gg, nrow = 2, ncol = 3)