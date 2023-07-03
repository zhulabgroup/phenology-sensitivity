# calculate the lag of Quercus

quercus <- read_rds("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/Quercus.rds")

  
data_qc <- quercus %>%   filter(observed_status_conflict_flag == "-9999") %>% # Removing Status Conflicts
  group_by(individual_id, first_yes_year, pheno_class_id) %>%
  arrange(first_yes_doy) %>%
  slice(1)%>%
  mutate(numdays_since_prior_no = na_if(numdays_since_prior_no, "-9999")) %>% # set the -9999 values to NA
  filter(numdays_since_prior_no < 20) %>% # Filtering Data by Prior No
  dplyr::select(individual_id, first_yes_year, first_yes_doy, species_id, dataset_id, pheno_class_id)

joined_data <- data_qc %>%
  filter(pheno_class_id == 1) %>%
  inner_join(data_qc %>%
               filter(pheno_class_id == 7),
             by = c("individual_id", "first_yes_year", "species_id", "dataset_id")) %>% 
  dplyr::select('first_yes_doy.x', 'first_yes_doy.y','species_id') %>% 
  rename(leaf = first_yes_doy.x, flower = first_yes_doy.y) %>% 
  mutate(lag = flower-leaf) %>% 
  group_by(species_id) %>% 
  filter(n()>30)
  
joined_data %>%
  ggplot(aes(x = lag, y = as.factor(species_id))) +
  geom_violinhalf() +
  geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.1) +
  geom_boxplot(width=0.1)

joined_data %>% ggplot(joined_data, aes(x = lag, y = as.factor(species_id))) +
  geom_violin()
  geom_jitter(shape=16, position=position_jitter(0.2))

joined_data %>% 
  ggplot() +
  geom_jitter(aes(x = leaf, y = flower), shape=16, position = position_jitter(0.2))