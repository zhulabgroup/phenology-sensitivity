# calculate the lag of Quercus for npn
source("scripts/function_get_clean_npn_first.R")

species_code <- rnpn::npn_species() %>% 
  dplyr::select('functional_type', 'common_name', 'species_id')

quercus <- read_rds("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/Quercus.rds")%>% 
  get_clean_npn_first() %>% 
  rename(leaf = first_yes_doy.x, flower = first_yes_doy.y) %>% 
  mutate(lag = flower-leaf)

# get climate data
raster_pre <- terra::rast("/nfs/turbo/seas-zhukai/climate/CHELSA/climatology/bio12.tif")

# connect them
joined_data_name <- quercus %>% 
  left_join(species_code, by = "species_id") %>% 
  mutate(pre = terra::extract(raster_pre, cbind(longitude, latitude))$bio12) 

#plot rlm
oak_rlm <- ggplot(joined_data_name,aes(x = leaf, y = flower)) +
  geom_point(alpha = 0.1) +
  # geom_smooth(method = "rlm",aes(x = leaf, y = flower), se = FALSE) +
  xlab("Leafing Out Day") +
  ylab("Flowering Day") +
  facet_wrap(~ common_name)+
  geom_abline(intercept = 0, slope = 1)
  

# plot lag violin  
# Calculate mean and standard deviation
summarised_data <- joined_data_name %>%
  group_by(common_name, functional_type) %>%
  summarise(mean_lag = mean(lag, na.rm = TRUE), 
            sd_lag = sd(lag, na.rm = TRUE))
# Draw the plot
oak_violin <- joined_data_name %>%
  ggplot(aes(y = lag, x = reorder(common_name, lag, FUN = mean))) +
  geom_violinhalf(aes(fill = "green"), position = position_nudge(x = .2, y = 0), show.legend = FALSE) +
  geom_jitter(alpha = 0.1, width = 0.15) +
  geom_point(data = summarised_data, aes(y = mean_lag, 
                                         x = reorder(common_name, mean_lag)),shape = 16, size = 4, color = "red", fill = "white") +
  geom_errorbar(data = summarised_data, 
                aes(ymin = mean_lag - sd_lag, ymax = mean_lag + sd_lag, x = reorder(common_name, mean_lag)), 
                width = 0.2, color = 'red', inherit.aes = FALSE) +
  xlab("") +
  ylab("Flower day - Leafing day") +
  scale_y_continuous(limits = c(-20, 50)) +
  coord_flip() +
  theme_bw()

  
  
# plot leaf_flower precipitation figure 
flower_leaf <- joined_data_name %>% 
  group_by(common_name) %>% 
  summarise(avepre = mean(pre),
            aveflower = mean(flower),
            aveleaf = mean(leaf)) %>%
  ggplot() +
  geom_point(aes(x = avepre, y = aveflower, color = "flower"))+
  geom_point(aes(x = avepre, y = aveleaf, color = "leaf"))+
  geom_smooth(aes(x = avepre, y = aveflower, color = "flower"), method = "lm", se = FALSE) +
  geom_smooth(aes(x = avepre, y = aveleaf, color = "leaf"), method = "lm", se = FALSE) +
  xlab(expression("Average Precipitation (kg m"^-2*" yr"^-1*")")) +  # Corrected x-axis label
  ylab("Day of Year") +
  theme_bw() +
  theme(legend.position = c(.2, .8)) +
  labs(color = NULL)
  

# plot lag_pre
lag_pre <- joined_data_name %>% 
  group_by(common_name) %>% 
  summarise(avepre = mean(pre),
            avelag = mean(lag)) %>%
  ggplot(aes(x = avepre, y = avelag)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) +
  ggrepel::geom_text_repel(aes(label = common_name), hjust = 0, vjust = 0) +
  xlab(expression("Average Precipitation (kg m"^-2*" yr"^-1*")")) +  # Corrected x-axis label
  ylab("Average Lag Day") +
  theme_bw()


plot <- oak_violin+flower_leaf+lag_pre+
  patchwork::plot_annotation(tag_levels = "A") +
  patchwork::plot_layout(design = 
                           "AB
                            AC
                            AC"
  )

ggsave("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/plot.png", plot, width = 10, height = 8)


# report species level lag
lag_table <- joined_data_name %>% 
  group_by(common_name,species_id) %>% 
  summarise(avelag = mean(lag))

write_csv(lag_table,"/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/oak_species_lag.csv")
