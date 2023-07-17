# show the detail of the flower and leaf data & the effect of the lag

sample_staiton <- points_within_buffer_filtered[["5effe609-c645-4620-bc99-a3b34934897c"]]

sample_staiton_uni <- sample_staiton %>% 
  unique() %>%
  group_by(year, doy, species_id) %>%
  filter(n() == 1) %>%
  ungroup()

oak_station_insight <-
 
  sample_staiton_uni %>%
  ggplot() + 
  geom_tile(aes(x = doy, y = as.factor(individual_id), fill = factor(phenophase_status))) +
  scale_fill_manual(values = c("grey", "brown")) +
  labs(x = "Day of year", y = "Year") + 
  facet_wrap(~ year)


  
  
  
  
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

