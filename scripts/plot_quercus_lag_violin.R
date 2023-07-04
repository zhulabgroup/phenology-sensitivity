# calculate the lag of Quercus
invisible({
library(see)

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
  filter(abs(lag)<183) %>% 
  group_by(species_id) %>% 
  filter(n()>30) %>% 
  ungroup()




  
p <- joined_data %>%
  ggplot(aes(y = lag, x = as.factor(species_id))) +
  geom_violinhalf(position = position_nudge(x = .2, y = 0)) +
  geom_jitter(alpha = 0.1, width = 0.15) +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 4, color = "red", fill = "white") +
  stat_summary(fun.data = "mean_sdl", geom = "errorbar", width = 0.2, color = "red", fun.args = list(mult = 1)) +
  xlab("species_id")+
  ylab("flower day - leafing day") +
  theme(legend.position = "none") 

p2 <- p +
  scale_y_continuous(limits = c(-20, 50))

p3 <- joined_data %>%
  ggplot(aes(x = "Quercus", y = lag)) +
  geom_violinhalf(position = position_nudge(x = .2, y = 0)) +
  geom_jitter(alpha = 0.1, width = 0.15) +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 4, color = "red", fill = "white") +
  stat_summary(fun.data = "mean_sdl", geom = "errorbar", width = 0.2, color = "red", fun.args = list(mult = 1)) +
  xlab("")+
  ylab("flower day - leafing day") +
theme(legend.position = "none") +
  scale_y_continuous(limits = c(-20, 50))



lags_violin <- p + p2 + p3 +
  patchwork::plot_annotation(tag_levels = "A") +
  patchwork::plot_layout(design = 
  "AAAAAC
  BBBBBC"
          )
  
})

