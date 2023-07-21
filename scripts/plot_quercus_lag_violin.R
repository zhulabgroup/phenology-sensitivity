# calculate the lag of Quercus for npn
source("scripts/function_get_clean_npn_first.R")

species_code <- rnpn::npn_species() %>% 
  dplyr::select('functional_type', 'common_name', 'species_id')

quercus <- read_rds("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/Quercus.rds")%>% 
  get_clean_npn_first() %>% 
  rename(leaf = first_yes_doy.x, flower = first_yes_doy.y) %>% 
  mutate(lag = flower-leaf)

# get climate data
raster_tem <- terra::rast("/nfs/turbo/seas-zhukai/climate/CHELSA/climatology/bio1.tif")
raster_pre <- terra::rast("/nfs/turbo/seas-zhukai/climate/CHELSA/climatology/bio12.tif")

# connect them
joined_data_name <- quercus %>% 
  left_join(species_code, by = "species_id") %>% 
  mutate(tem = terra::extract(raster_tem, cbind(longitude, latitude))$bio1,
         pre = terra::extract(raster_pre, cbind(longitude, latitude))$bio12) 



chatgpt_output <- joined_data_name %>% 
  select(tem, pre, common_name, flower, lag, leaf)

write_csv(chatgpt_output,"/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/trygpt.csv")



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
  ggplot(aes(y = lag, x = reorder(common_name, lag, FUN = mean), fill = functional_type)) +
  geom_violinhalf(position = position_nudge(x = .2, y = 0)) +
  geom_jitter(alpha = 0.1, width = 0.15) +
  geom_point(data = summarised_data, aes(y = mean_lag, 
                                         x = reorder(common_name, mean_lag)),shape = 16, size = 4, color = "red", fill = "white") +
  geom_errorbar(data = summarised_data, 
                aes(ymin = mean_lag - sd_lag, ymax = mean_lag + sd_lag, x = reorder(common_name, mean_lag)), 
                width = 0.2, color = 'red', inherit.aes = FALSE) +
  xlab("Species") +
  ylab("flower day - leafing day") +
  scale_y_continuous(limits = c(-20, 50)) +
  coord_flip() +
  labs(fill = "Functional Type")  
  
  
 

##################
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
  ggplot(selected_species,aes(x = pre$bio12, y = leaf)) +
  geom_point(alpha = 0.1) +
  # geom_smooth(method = "rlm",aes(x = leaf, y = flower), se = FALSE) +
  xlab("Temperature") +
  facet_wrap(~ common_name, nrow = 1)
q2 <- 
  ggplot(selected_species,aes(x = pre$bio12, y = flower)) +
  geom_point(alpha = 0.1) +
  # geom_smooth(method = "rlm",aes(x = leaf, y = flower), se = FALSE) +
  xlab("Temperature") +
  facet_wrap(~ common_name, nrow = 1)
q3 <- 
  ggplot(selected_species,aes(x = pre$bio12, y = lag)) +
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

q1 <- ggplot(joined_data_name,aes(x = pre$bio12, y = leaf, color = common_name)) +
  geom_point(alpha = 0.1) +
  # geom_smooth(method = "rlm",aes(x = leaf, y = flower), se = FALSE) +
  xlab("Temperature") 

q2 <- ggplot(joined_data_name,aes(x = pre$bio12, y = flower, color = common_name)) +
  geom_point(alpha = 0.1) +
  # geom_smooth(method = "rlm",aes(x = leaf, y = flower), se = FALSE) +
  xlab("Temperature") 

q3 <- ggplot(joined_data_name,aes(x = pre$bio12, y = lag, color = common_name)) +
  geom_point(alpha = 0.1) +
  # geom_smooth(method = "rlm",aes(x = leaf, y = flower), se = FALSE) +
  xlab("Temperature") 

q1+q2+q3+
  patchwork::plot_annotation(tag_levels = "A") +
  patchwork::plot_layout(design = 
                           "ABC"
  )

joined_data_name %>% 
  group_by(common_name) %>% 
  summarise(avelag = mean(lag),
            avetem = mean(tem$bio1),
            avepre = mean(pre$bio12)) %>% 
  ggplot(aes(x = avepre, y = avelag)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) +
  ggrepel::geom_text_repel(aes(label = common_name), hjust = 0, vjust = 0)

# report species level lag
lag_table <- joined_data_name %>% 
  group_by(common_name,species_id) %>% 
  summarise(avelag = mean(lag))

write_csv(lag_table,"/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/oak_species_lag.csv")
