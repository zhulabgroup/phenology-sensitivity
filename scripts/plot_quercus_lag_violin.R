# calculate the lag of Quercus
library(rnpn)
species_code <- rnpn::npn_species() %>% 
  dplyr::select('functional_type', 'common_name', 'species_id')

quercus <- read_rds("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/Quercus.rds")

raster_data <- terra::rast("/nfs/turbo/seas-zhukai/climate/CHELSA/climatology/bio1.tif")

data_qc <- quercus %>%   filter(observed_status_conflict_flag == "-9999") %>% # Removing Status Conflicts
  group_by(individual_id, first_yes_year, pheno_class_id) %>%
  arrange(first_yes_doy) %>%
  slice(1)%>%
  mutate(numdays_since_prior_no = na_if(numdays_since_prior_no, "-9999")) %>% # set the -9999 values to NA
  filter(numdays_since_prior_no < 20) %>% # Filtering Data by Prior No
  dplyr::select(individual_id, first_yes_year, first_yes_doy, species_id, dataset_id, pheno_class_id, longitude, latitude)


joined_data <- data_qc %>%
  filter(pheno_class_id == 1) %>%
  inner_join(data_qc %>%
               filter(pheno_class_id == 7),
             by = c("individual_id", "first_yes_year", "species_id", "dataset_id", "longitude", "latitude")) %>% 
  dplyr::select('first_yes_doy.x', 'first_yes_doy.y','species_id', "longitude", "latitude") %>% 
  rename(leaf = first_yes_doy.x, flower = first_yes_doy.y) %>% 
  mutate(lag = flower-leaf) %>% 
  filter(abs(lag)<183) %>% 
  group_by(species_id) %>% 
  filter(n()>30) %>% 
  ungroup()

joined_data_name <- joined_data %>% 
  left_join(species_code, by = "species_id") %>% 
  mutate(tem = terra::extract(raster_data, cbind(longitude, latitude))) 

#plot rlm
oak_rlm <- ggplot(joined_data_name,aes(x = leaf, y = flower)) +
  geom_point(alpha = 0.1) +
  # geom_smooth(method = "rlm",aes(x = leaf, y = flower), se = FALSE) +
  xlab("Leafing Out Day") +
  ylab("Flowering Day") +
  facet_wrap(~ common_name)+
  geom_abline(intercept = 0, slope = 1)
  
  

# plot lag violin  
p <- joined_data_name %>%
  ggplot(aes(y = lag, x = common_name, fill = functional_type)) +
  geom_violinhalf(position = position_nudge(x = .2, y = 0)) +
  geom_jitter(alpha = 0.1, width = 0.15) +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 4, color = "red", fill = "white") +
  stat_summary(fun.data = "mean_sdl", geom = "errorbar", width = 0.2, color = 'red', fun.args = list(mult = 1)) +
  xlab("Species")+
  ylab("flower day - leafing day") +
  coord_flip() +
  scale_y_continuous(limits = c(-20, 50)) +
  labs(fill = "Functional Type")  

p3 <- joined_data %>%
  ggplot(aes(x = "Quercus", y = lag)) +
  geom_violinhalf(position = position_nudge(x = .2, y = 0)) +
  geom_jitter(alpha = 0.1, width = 0.15) +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 4, color = "red", fill = "white") +
  stat_summary(fun.data = "mean_sdl", geom = "errorbar", width = 0.2, color = "red", fun.args = list(mult = 1)) +
  xlab("")+
  ylab("flower day - leafing day") +
  scale_y_continuous(limits = c(-20, 50))+
  coord_flip()



lags_violin <- p + p3 +
  patchwork::plot_annotation(tag_levels = "A") +
  patchwork::plot_layout(design = 
  "A
  A
  A
  A
  A
  A
  A
  B"
          )

# plot oak lag-tem
selected_species <- joined_data_name %>% 
  filter(common_name %in% c("bur oak", "chestnut oak","nothern red oak","pin oak","white oak"))


q1 <- 
  ggplot(selected_species,aes(x = tem$bio1, y = leaf)) +
  geom_point(alpha = 0.1) +
  # geom_smooth(method = "rlm",aes(x = leaf, y = flower), se = FALSE) +
  xlab("Temperature") +
  facet_wrap(~ common_name, nrow = 1)
q2 <- 
  ggplot(selected_species,aes(x = tem$bio1, y = flower)) +
  geom_point(alpha = 0.1) +
  # geom_smooth(method = "rlm",aes(x = leaf, y = flower), se = FALSE) +
  xlab("Temperature") +
  facet_wrap(~ common_name, nrow = 1)
q3 <- 
  ggplot(selected_species,aes(x = tem$bio1, y = lag)) +
  geom_point(alpha = 0.1) +
  # geom_smooth(method = "rlm",aes(x = leaf, y = flower), se = FALSE) +
  xlab("Temperature") +
  facet_wrap(~ common_name, nrow = 1)

q1 + q2 + q3 +
  patchwork::plot_annotation(tag_levels = "A") +
  patchwork::plot_layout(design = 
                           "A
                           B
                           C"
  )