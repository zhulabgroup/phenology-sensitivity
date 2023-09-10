source("scripts/function_get_clean_npn_first.R")
source("scripts/function_download_daymet_seasonal_tem.R")

quercus <- read_rds("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/Quercus.rds")%>% 
  get_clean_npn_first() %>% 
  rename(leaf = first_yes_doy.x, flower = first_yes_doy.y, year = first_yes_year) %>% 
  mutate(lag = flower-leaf)

quercus_tem <- quercus %>% 
  dplyr::select("latitude", "longitude", "year") %>% 
  filter(year<2023) %>% 
  get_winter_spring_temperatures()

species_code <- rnpn::npn_species() %>% 
  dplyr::select('functional_type', 'common_name', 'species_id') 

joined_data <- inner_join(quercus_tem, quercus, by = c("latitude", "longitude", "year")) %>% 
  unique() %>% 
  left_join(species_code, by = "species_id")

write_rds(joined_data, "/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/Quercus_winsprtem.rds")
  