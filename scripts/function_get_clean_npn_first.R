# data pre process for npn first day data

get_clean_npn_first <- function(taxadata){
  
data_qc <- taxadata %>%   filter(observed_status_conflict_flag == "-9999") %>% # Removing Status Conflicts
  group_by(individual_id, first_yes_year, pheno_class_id) %>%
  arrange(first_yes_doy) %>%
  slice(1)%>%
  mutate(numdays_since_prior_no = na_if(numdays_since_prior_no, "-9999")) %>% # set the -9999 values to NA
  filter(numdays_since_prior_no < 20) %>% # Filtering Data by Prior No
  dplyr::select(individual_id, first_yes_year, first_yes_doy, species_id, dataset_id, pheno_class_id, longitude, latitude) %>% 
  ungroup()

# visial check the normal distribution
# data_qc %>%
#    group_by(pheno_class_id, species_id) %>%
#    filter(n() > 30) %>%
#    ungroup() %>%
#    ggplot() +
#    geom_histogram(aes(x = first_yes_doy), bins = 30) +
#    facet_grid(species_id ~ pheno_class_id)

data_qc_outlier <- data_qc %>%
  group_by(pheno_class_id, species_id) %>%
  filter(n() > 30) %>%
  summarise(aver = mean(first_yes_doy),
            std = sd(first_yes_doy),
            lower = aver - 1.645 * std,
            upper = aver + 1.645 * std) %>%
  inner_join(data_qc, by = c("pheno_class_id", "species_id")) %>%
  mutate(first_yes_year = if_else(first_yes_doy > upper, first_yes_year + 1, as.double(first_yes_year)),
         first_yes_doy = if_else(first_yes_doy > upper, first_yes_doy - 365, as.double(first_yes_doy) )) %>% 
  filter(first_yes_doy > lower) %>%
  ungroup() %>% 
  dplyr::select(-aver,-std,-lower,-upper)

joined_data <- data_qc_outlier %>%
  filter(pheno_class_id == 1) %>%
  inner_join(data_qc_outlier %>%
               filter(pheno_class_id == 7),
             by = c("individual_id", "first_yes_year", "species_id", "dataset_id", "longitude", "latitude")) %>% 
  group_by(species_id) %>% 
  filter(n()>30) %>% 
  ungroup()

return(joined_data)

}