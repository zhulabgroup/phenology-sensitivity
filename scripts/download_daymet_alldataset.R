path_npn <- "/nfs/turbo/seas-zhukai/phenology/NPN/individual_phenometrics/leaf_flower/with_climate/"
# path_npn <- "/Volumes/seas-zhukai/phenology/NPN/individual_phenometrics/leaf_flower/with_climate/"


oak <- read_rds(str_c(path_npn, "Quercus", ".rds")) #55079

neon_oak <- oak %>% 
  filter(observed_status_conflict_flag == "-9999") %>% # 51863
  group_by(individual_id, first_yes_year, pheno_class_id)  %>%
  filter(n() == 1) %>%  # Keep only groups with one observation 26913
  ungroup()  %>%
  mutate(numdays_since_prior_no = as.numeric(numdays_since_prior_no)) %>%
  filter(numdays_since_prior_no > 0) %>% # Filtering Data by Prior No 18881
  filter(numdays_since_prior_no < 30) %>% # Filtering Data by Prior No 17360
  dplyr::select(individual_id, first_yes_year, first_yes_doy, species_id, dataset_id, pheno_class_id, longitude, latitude) %>% 
  ungroup() %>% 
  rename(year = first_yes_year)

ggplot(neon_oak, aes(x = as.factor(pheno_class_id))) +
  geom_bar() +
  labs(x = "Pheno Class ID", y = "Count") +
  ggtitle("Boxplot of Counts by Pheno Class ID")  #4000 in total vs 750 in neon
# 1 has the most observation and can show leaf
# 7 has the most for flower
# so we are using the same phase with neon

# prepare tem for all locations for both leaf and flower
source("scripts/function_download_daymet_seasonal_tem.R")

quercus_tem <- neon_oak %>% 
  dplyr::select("latitude", "longitude", "year") %>% 
  unique() %>% 
  filter(year<2023) %>% # no 2023 data for neon
  get_winter_spring_temperatures() # 2151 vs 79 locations in neon

species_code <- rnpn::npn_species() %>% 
  dplyr::select('functional_type', 'common_name', 'species_id') 

# join data
joined_data_leaf <- neon_oak %>% 
  filter(pheno_class_id == 1) %>% 
  inner_join(quercus_tem, by = c("latitude", "longitude", "year")) %>% 
  unique() %>% 
  left_join(species_code, by = "species_id") # 3720 vs 894 in neon

joined_data_flower <- neon_oak %>% 
  filter(pheno_class_id == 7) %>% 
  inner_join(quercus_tem, by = c("latitude", "longitude", "year")) %>% 
  unique() %>% 
  left_join(species_code, by = "species_id") # 2227 vs 517 in neon

write_rds(joined_data_leaf, "/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/neon/Quercus_leaf_winsprtem_all.rds")
write_rds(joined_data_flower, "/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/neon/Quercus_flower_winsprtem_all.rds")
