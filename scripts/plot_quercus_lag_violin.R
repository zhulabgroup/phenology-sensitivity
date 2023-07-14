# calculate the lag of Quercus
source("scripts/function_get_clean_npn_first.R")

species_code <- rnpn::npn_species() %>% 
  dplyr::select('functional_type', 'common_name', 'species_id')

quercus <- read_rds("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/Quercus.rds")%>% 
  get_clean_npn_first() %>% 
  rename(leaf = first_yes_doy.x, flower = first_yes_doy.y) %>% 
  mutate(lag = flower-leaf)


raster_tem <- terra::rast("/nfs/turbo/seas-zhukai/climate/CHELSA/climatology/bio1.tif")
raster_pre <- terra::rast("/nfs/turbo/seas-zhukai/climate/CHELSA/climatology/bio12.tif")


joined_data_name <- quercus %>% 
  left_join(species_code, by = "species_id") %>% 
  mutate(tem = terra::extract(raster_tem, cbind(longitude, latitude)),
         pre = terra::extract(raster_pre, cbind(longitude, latitude))) 


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
  ggplot(aes(y = lag, x = reorder(common_name, lag, FUN = mean), fill = functional_type)) +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 4, color = "red", fill = "white") +
  stat_summary(fun.data = "mean_sdl", geom = "errorbar", width = 0.2, color = 'red', fun.args = list(mult = 1)) +
  geom_violinhalf(position = position_nudge(x = .2, y = 0)) +
  geom_jitter(alpha = 0.1, width = 0.15) +
  xlab("Species") +
  ylab("flower day - leafing day") +
  scale_y_continuous(limits = c(-20, 50)) +
  coord_flip() +
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

  
