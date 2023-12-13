source("scripts/function_download_daymet_seasonal_tem.R")

folder_path <- "/nfs/turbo/seas-zhukai/phenology/NPN/individual_phenometrics/leaf_flower/"

# Get list of RDS files in the folder
rds_files <- list.files(path = folder_path, pattern = "\\.rds$", full.names = TRUE)

# Initialize an empty data frame to store the results
final_list <- data.frame()

for (i in seq_along(rds_files)) {
  maple <- readRDS(rds_files[i])
  
  climate <- maple %>%
    select("latitude", "longitude", "first_yes_year") %>%
    unique() %>%
    filter(first_yes_year < 2023)
  
  final_list <- bind_rows(final_list, climate)
}

final_list_climate <- final_list %>% 
  unique() %>% # 17156->11236
  rename(year = first_yes_year) %>% 
  get_winter_spring_temperatures()

write_rds(final_list_climate,"data/different_species/climate_list.rds") 

  