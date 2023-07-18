# show the detail of the flower and leaf data & the effect of the lag

sample_staiton <- points_within_buffer_filtered[["5effe609-c645-4620-bc99-a3b34934897c"]]

sample_staiton_uni <- sample_staiton %>% 

  mutate(individual_id <- as.factor(individual_id),
         species_id <- as.factor(species_id))

oak_station_insight <-

  

sample_staiton_uni %>%
  filter(year==2022) %>% 
  ggplot() + 
  geom_tile(aes(x = doy, y = reorder(individual_id, species_id), fill = factor(phenophase_status))) +
  scale_fill_manual(values = c("grey", "brown")) +  
  ylab('individual') +
  theme(
        axis.text.y = element_blank()
        ) +
  ggtitle("5effe609-c645-4620-bc99-a3b34934897c year2022")

  
  
  
  
  ggplot(aes(x = doy, y = reorder(individual_id, species_id)),color = species_id, shape = factor(phenophase_status)) +
  geom_point(size = 1) +
  scale_color_discrete() +
  scale_shape_manual(values = c(21, 22)) +
  facet_wrap(~ year) +
  labs(x = "Day of Year", y = "Individual ID") +
  theme_bw()






zero_counts <- sample_staiton %>%
  group_by(year,doy,species_id) %>%
  summarise(yes_count = sum(phenophase_status),
            total_count = n())

ggplot(zero_counts) +
  geom_line(aes(x = doy, y = yes_count), color = 'green') +
  geom_line(aes(x = doy, y = total_count)) +
  facet_wrap(species_id ~ year) +
  xlab("Year") +
  ylab("Counts")

