library(rnpn)
library(tidyverse)

list_all_focal_taxa <- c(62,63,319)


leafing <- npn_download_individual_phenometrics(
  request_source = 'Daniel Katz, Cornell and/or Theresa Crimmins',
  species_ids = list_all_focal_taxa,
  years = c(as.character(2012:2022)), # years to include
  pheno_class_ids = c(1, 2) 
) %>%
  filter(numdays_since_prior_no >= 0 & numdays_since_prior_no <= 7) %>%
  select(individual_id, first_yes_year, first_yes_doy, pheno_class_id) %>%
  rename(id = individual_id, year = first_yes_year, leafing_day = first_yes_doy, pheno = pheno_class_id)%>%
  group_by(id, year, pheno) %>%
  filter(n_distinct(leafing_day) == 1) %>%
  ungroup()


flowering <- npn_download_individual_phenometrics(
  request_source = 'Daniel Katz, Cornell and/or Theresa Crimmins',
  species_ids = list_all_focal_taxa,
  years = c(as.character(2012:2022)), # years to include
  pheno_class_ids = c(6, 7) 
) %>%
  filter(numdays_since_prior_no >= 0 & numdays_since_prior_no <= 7) %>%
  select(individual_id, first_yes_year, first_yes_doy, pheno_class_id) %>%
  rename(id = individual_id, year = first_yes_year, flowering_day = first_yes_doy, pheno = pheno_class_id)%>%
  group_by(id, year, pheno) %>%
  filter(n_distinct(flowering_day) == 1) %>%
  ungroup()

joined_data <- leafing %>%
  filter(pheno == 1) %>%
  full_join(flowering %>% filter(pheno == 7), by = c("id", "year")) %>%
  filter(complete.cases(.))


ggplot(joined_data, aes(x = leafing_day, y = flowering_day)) +
  geom_point() +
  xlab("Leafing Day") +
  ylab("Flowering Day") +
  ggtitle("Leafing vs. Flowering") +
  facet_wrap(~ id)

