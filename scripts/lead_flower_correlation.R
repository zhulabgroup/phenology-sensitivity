library(rnpn)
library(tidyverse)

phenophases <- npn_phenophases() # get pheno_class


down_clean_npn <- function(species, year, pheno_class, error, dataset) {
  phenoday <- npn_download_individual_phenometrics(
    request_source = 'YL',
    species_ids = species,
    years = year, # years to include
    pheno_class_ids = pheno_class, 
    dataset_ids = dataset
  ) %>%
    group_by(individual_id, first_yes_year, pheno_class_id) %>%
    filter(first_yes_doy == min(first_yes_doy)) %>%
    ungroup() %>%
    filter(numdays_since_prior_no >= 0 & numdays_since_prior_no <= error) %>%
    select(individual_id, first_yes_year, first_yes_doy, pheno_class_id) %>%
    rename(id = individual_id, year = first_yes_year, doy = first_yes_doy, pheno = pheno_class_id)
  
  # First step: identify the ids that should be removed
  ids_to_remove <- phenoday %>%
    group_by(id, year) %>%
    mutate(num = n()) %>%
    filter(num > 1 & any(diff(sort(doy)) > error)) %>%
    pull(id) %>%
    unique()

  # Second step: filter out the groups with the ids identified in the first step
  phenoday <- phenoday %>%
    filter(!(id %in% ids_to_remove)) %>%
    group_by(id, year) %>%
    mutate(num = n()) %>%
    filter(doy == min(doy)) %>%
    slice(1) %>%
    select(-num)
  # Return the result
return(phenoday)
}
list_all_focal_taxa <- c(62,63,319)

# list_all_focal_taxa <- c(1 , 778 ,1591 ,   2 ,1843 ,1199 , 779  ,  3  , 59 , 781 ,  60 ,  61 , 780 , 777)

flowering <- down_clean_npn(list_all_focal_taxa, c(as.character(2020:2022)), c(6, 7, 8),7,16)
leafing <- down_clean_npn(list_all_focal_taxa, c(as.character(2020:2022)), c(1, 2, 3),7)


joined_data <- inner_join(leafing, flowering, by = c("id", "year")) 


ggplot(joined_data, aes(x = doy.x, y = doy.y)) +
  geom_jitter() +
  xlab("Leafing Day") +
  ylab("Flowering Day") +
  ggtitle("Leafing vs. Flowering") +
  geom_abline(intercept = 0, slope = 1) +
  coord_equal()
