path_npn <- "/nfs/turbo/seas-zhukai/phenology/NPN/individual_phenometrics/leaf_flower/with_climate/"

oak <- read_rds(str_c(path_npn, "Quercus", ".rds"))

neon_oak <- oak %>% 
  filter(partner_group == "National Ecological Observatory Network (NEON)") %>%  
  filter(observed_status_conflict_flag == "-9999") %>% # Removing Status Conflicts
  group_by(individual_id, first_yes_year, pheno_class_id) %>%
  arrange(first_yes_doy) %>%
  slice(1)%>% # delete ~3000 observation
  mutate(numdays_since_prior_no = as.numeric(numdays_since_prior_no)) %>%
  filter(numdays_since_prior_no < 20 & numdays_since_prior_no > 0) %>% # Filtering Data by Prior No
  dplyr::select(individual_id, first_yes_year, first_yes_doy, species_id, dataset_id, pheno_class_id, longitude, latitude) %>% 
  ungroup()

first_five_groups <- oak %>% 
  filter(partner_group == "National Ecological Observatory Network (NEON)") %>%  
  filter(observed_status_conflict_flag == "-9999") %>% # Removing Status Conflicts
  group_by(individual_id, first_yes_year, pheno_class_id) %>%
  arrange(first_yes_doy) %>%
  slice_head(n = 1)




group_counts <- oak %>%
  filter(partner_group == "National Ecological Observatory Network (NEON)") %>%
  filter(observed_status_conflict_flag == "-9999") %>%
  group_by(individual_id, first_yes_year, pheno_class_id) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(count > 1)  # Keep only groups with more than one observation

# Choose a specific group (e.g., the first one)
chosen_group <- group_counts %>% slice_head(n = 1)

selected_group_data <- oak %>%
  filter(
    individual_id == chosen_group$individual_id,
    first_yes_year == chosen_group$first_yes_year,
    pheno_class_id == chosen_group$pheno_class_id
  ) %>%
  arrange(first_yes_doy) 

