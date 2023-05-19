acer <- c(62,63,319)

npn <- npn_download_individual_phenometrics(
  request_source = 'LY',
  species_ids = acer,
  years = c(as.character(2012:2022)), # years to include
  pheno_class_ids = c(1, 2, 3, 6,7,8),
  additional_fields = c("ObservedBy_Person_ID","Observed_Status_Conflict_Flag","multiple_firsty","partner_group")
)



npn_simple <- npn %>%  select(individual_id, first_yes_year, first_yes_doy, pheno_class_id, multiple_firsty, phenophase_description, observedby_person_id,observed_status_conflict_flag,numdays_since_prior_no) %>% 
  filter(observed_status_conflict_flag == "-9999") %>% # Removing Status Conflicts
  group_by(individual_id, first_yes_year, pheno_class_id) %>%
  filter(first_yes_doy == min(first_yes_doy)) %>%
  ungroup() %>%
  mutate(numdays_since_prior_no = na_if(numdays_since_prior_no, "-9999")) %>% # set the -9999 values to NA
  filter(numdays_since_prior_no < 8)# Filtering Data by Prior No

leafing <- npn_simple %>% filter(pheno_class_id == 1) %>% 
  select(individual_id, first_yes_year, first_yes_doy)

flowering <- npn_simple %>% filter(pheno_class_id == 6)%>% 
  select(individual_id, first_yes_year, first_yes_doy)

joined_data <- inner_join(leafing, flowering, by = c("individual_id", "first_yes_year") )

ggplot(joined_data, aes(x = first_yes_doy.x, y = first_yes_doy.y)) +
  geom_jitter() +
  xlab("Leafing Day") +
  ylab("Flowering Day") +
  ggtitle("Leafing vs. Flowering") +
  geom_abline(intercept = 0, slope = 1) +
  coord_equal()

###########
quercus <- c(705,100,1365,757,1870,987,1690,1484,988,316,297,1485,1190,765,1486,
             301,704,101,1691,1212,989,1366,102,1756,1213,1755,1487,1159,305)

npn_quercus <- npn_download_individual_phenometrics(
  request_source = 'LY',
  species_ids = quercus,
  years = c(as.character(2012:2022)), # years to include
  pheno_class_ids = c(1,6),
  additional_fields = c("ObservedBy_Person_ID","Observed_Status_Conflict_Flag","multiple_firsty","partner_group")
)

npn_simple <- npn_quercus %>%   filter(observed_status_conflict_flag == "-9999") %>% # Removing Status Conflicts
  group_by(individual_id, first_yes_year, pheno_class_id) %>%
  filter(first_yes_doy == min(first_yes_doy)) %>%
  ungroup() %>%
  mutate(numdays_since_prior_no = na_if(numdays_since_prior_no, "-9999")) %>% # set the -9999 values to NA
  filter(numdays_since_prior_no < 8)# Filtering Data by Prior No

leafing <- npn_simple %>% filter(pheno_class_id == 1) %>% 
  select(individual_id, first_yes_year, first_yes_doy, species_id)

flowering <- npn_simple %>% filter(pheno_class_id == 6)%>% 
  select(individual_id, first_yes_year, first_yes_doy, species_id)

joined_data <- inner_join(leafing, flowering, by = c("individual_id", "first_yes_year", "species_id") )

ggplot(joined_data) +
  geom_point(aes(x = first_yes_doy.x, y = first_yes_doy.y, color = as.factor(species_id) ), alpha = 0.5) +
  xlab("Leafing Day") +
  ylab("Flowering Day") +
  ggtitle("Leafing vs. Flowering") +
  geom_abline(intercept = 0, slope = 1) +
  coord_equal() 
