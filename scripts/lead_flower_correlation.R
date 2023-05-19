library(tidyverse)
library(rnpn)

acer <- c(62,63,319)

npn <- npn_download_individual_phenometrics(
  request_source = 'LY',
  species_ids = acer,
  years = c(as.character(2012:2022)), # years to include
  pheno_class_ids = c(1, 2, 3, 6,7,8),
  additional_fields = c("ObservedBy_Person_ID","Observed_Status_Conflict_Flag","multiple_firsty","partner_group","dataset_id")
)



npn_simple <- npn %>%  
  filter(observed_status_conflict_flag == "-9999") %>% # Removing Status Conflicts
  group_by(individual_id, first_yes_year, pheno_class_id) %>%
  filter(first_yes_doy == min(first_yes_doy)) %>%
  ungroup() %>%
  mutate(numdays_since_prior_no = na_if(numdays_since_prior_no, "-9999")) %>% # set the -9999 values to NA
  filter(numdays_since_prior_no < 8) %>% # Filtering Data by Prior No
  select(individual_id, first_yes_year, first_yes_doy, species_id, dataset_id,pheno_class_id)

leafing <- npn_simple %>% filter(pheno_class_id == 1)
flowering <- npn_simple %>% filter(pheno_class_id == 6)

joined_data <- inner_join(leafing, flowering, by = c("individual_id", "first_yes_year", "species_id", "dataset_id") )

ggplot(joined_data) +
  geom_point(aes(x = first_yes_doy.x, y = first_yes_doy.y, color = as.factor(species_id) ), alpha = 0.5) +
  xlab("Leafing Day") +
  ylab("Flowering Day") +
  ggtitle("Leafing vs. Flowering") +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~dataset_id)
  coord_equal() 

###########
quercus <- c(705,100,1365,757,1870,987,1690,1484,988,316,297,1485,1190,765,1486,
             301,704,101,1691,1212,989,1366,102,1756,1213,1755,1487,1159,305)

npn_quercus <- npn_download_individual_phenometrics(
  request_source = 'LY',
  species_ids = quercus,
  years = c(as.character(2012:2022)), # years to include
  pheno_class_ids = c(1,6),
  additional_fields = c("ObservedBy_Person_ID","Observed_Status_Conflict_Flag","multiple_firsty","partner_group","dataset_id")
)

npn_quercus <- read_rds("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/Quercus.rds")

npn_simple <- npn_quercus %>%   filter(observed_status_conflict_flag == "-9999") %>% # Removing Status Conflicts
  group_by(individual_id, first_yes_year, pheno_class_id) %>%
  filter(first_yes_doy == min(first_yes_doy)) %>%
  ungroup() %>%
  mutate(numdays_since_prior_no = na_if(numdays_since_prior_no, "-9999")) %>% # set the -9999 values to NA
  filter(numdays_since_prior_no < 20) %>% # Filtering Data by Prior No
  select(individual_id, first_yes_year, first_yes_doy, species_id, dataset_id,pheno_class_id)


leafing <- npn_simple %>% filter(pheno_class_id == 1)
flowering <- npn_simple %>% filter(pheno_class_id == 7)

joined_data <- inner_join(leafing, flowering, by = c("individual_id", "first_yes_year", "species_id", "dataset_id") )

ggplot(joined_data) +
  geom_point(aes(x = first_yes_doy.x, y = first_yes_doy.y, color = as.factor(species_id) ), alpha = 0.2) +
  xlab("Leafing Day") +
  ylab("Flowering Day") +
  ggtitle("Leafing vs. Flowering") +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~dataset_id)
  coord_equal() 


