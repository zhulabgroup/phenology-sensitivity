path_npn <- "/nfs/turbo/seas-zhukai/phenology/NPN/individual_phenometrics/leaf_flower/with_climate/"
# path_npn <- "/Volumes/seas-zhukai/phenology/NPN/individual_phenometrics/leaf_flower/with_climate/"


oak <- read_rds(str_c(path_npn, "Quercus", ".rds"))

neon_oak <- oak %>% 
  filter(partner_group == "National Ecological Observatory Network (NEON)") %>%  #8112
  filter(observed_status_conflict_flag == "-9999") %>% # Removing Status Conflicts (no conflict for neon)
  group_by(individual_id, first_yes_year, pheno_class_id)  %>%
  filter(n() == 1) %>%  # Keep only groups with one observation 3447, multiple_firsty == 0 can still keep some with 2 observations
  ungroup()  %>%
  mutate(numdays_since_prior_no = as.numeric(numdays_since_prior_no)) %>%
  filter(numdays_since_prior_no > 0) %>% # Filtering Data by Prior No 2832
  filter(numdays_since_prior_no < 30) %>% # Filtering Data by Prior No 2560
  dplyr::select(individual_id, first_yes_year, first_yes_doy, species_id, dataset_id, pheno_class_id, longitude, latitude) %>% 
  ungroup() %>% 
  rename(year = first_yes_year)

#more than one first yes means there is no in between: NNYYNYY

ggplot(neon_oak, aes(x = as.factor(pheno_class_id))) +
  geom_bar() +
  labs(x = "Pheno Class ID", y = "Count") +
  ggtitle("Boxplot of Counts by Pheno Class ID") # 1 has the most observation and can show leaf

# prepare tem for all locations for both leaf and flower
source("scripts/function_download_daymet_seasonal_tem.R")

quercus_tem <- neon_oak %>% 
  dplyr::select("latitude", "longitude", "year") %>% 
  unique() %>% 
  filter(year<2023) %>% # no 2023 data for neon
  get_winter_spring_temperatures()

species_code <- rnpn::npn_species() %>% 
  dplyr::select('functional_type', 'common_name', 'species_id') 

# join data
joined_data_leaf <- neon_oak %>% 
  filter(pheno_class_id == 1) %>% 
  inner_join(quercus_tem, by = c("latitude", "longitude", "year")) %>% 
  unique() %>% 
  left_join(species_code, by = "species_id")

joined_data_flower <- neon_oak %>% 
  filter(pheno_class_id == 7) %>% 
  inner_join(quercus_tem, by = c("latitude", "longitude", "year")) %>% 
  unique() %>% 
  left_join(species_code, by = "species_id")

write_rds(joined_data_leaf, "/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/neon/Quercus_leaf_winsprtem.rds")
write_rds(joined_data_flower, "/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/neon/Quercus_flower_winsprtem.rds")
