library(rnpn)
library(tidyverse)

phenophases <- npn_phenophases() # get pheno_class


down_clean_npn <- function(species, year, pheno_class, error) {
  phenoday <- npn_download_individual_phenometrics(
    request_source = 'Daniel Katz, Cornell and/or Theresa Crimmins',
    species_ids = species,
    years = year, # years to include
    pheno_class_ids = pheno_class 
  ) %>%
    filter(numdays_since_prior_no >= 0 & numdays_since_prior_no <= error) %>%
    select(individual_id, first_yes_year, first_yes_doy, pheno_class_id) %>%
    rename(id = individual_id, year = first_yes_year, doy = first_yes_doy, pheno = pheno_class_id)%>%
    group_by(id, year, pheno) %>%
    filter(n_distinct(doy) == 1) %>%
    ungroup() 
  
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

list_all_focal_taxa <- c(705,100,1365,757,1870,987,1690,1484,988,316,297,1485,1190,765,1486,
                                                 301,704,101,1691,1212,989,1366,102,1756,1213,1755,1487,1159,305)

flowering <- down_clean_npn(list_all_focal_taxa, c(as.character(2000:2022)), c(6, 7, 8),7)
leafing <- down_clean_npn(list_all_focal_taxa, c(as.character(2000:2022)), c(1, 2, 3),7)


joined_data <- inner_join(leafing, flowering, by = c("id", "year")) 


ggplot(joined_data, aes(x = doy.x, y = doy.y)) +
  geom_point() +
  xlab("Leafing Day") +
  ylab("Flowering Day") +
  ggtitle("Leafing vs. Flowering") +
  facet_wrap(~ id)

wide_table <- flowering %>%
  pivot_wider(names_from = pheno, values_from = doy)

