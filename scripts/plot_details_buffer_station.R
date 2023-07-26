# check the lag process:
# 1. why for most station, the lag is not obvious
# 2. why lag helped reduct outliers?
# "1afdaa42-0a1f-4610-b9cd-c30a1493a2c9" check why they are the same 12.44 lag
# "5effe609-c645-4620-bc99-a3b34934897c"
# 575c20d1-846d-4ae3-bdb1-8aed42847a4a to check advancing after lag
source("scripts/function_get_lagbufferednpn_percentage.R")
result <- get_buffle_npn_percentage(50,1,"Quercus",10)
npn_leaf <- result[[1]]
npn_leaf_raw <- result[[2]]

result2<- get_buffle_npn_percentage(50,1,"Quercus",10,TRUE)
npn_leaf_lag <- result2[[1]]
npn_leaf_lag_raw <- result2[[2]]


test <- npn_leaf_raw[["575c20d1-846d-4ae3-bdb1-8aed42847a4a"]] %>% 
  filter(year == 2020)

test_lag <- npn_leaf_lag_raw[["575c20d1-846d-4ae3-bdb1-8aed42847a4a"]] %>% 
  filter(year == 2020)

test1 <- npn_leaf %>% 
  filter(year == 2016 & station == "1afdaa42-0a1f-4610-b9cd-c30a1493a2c9")
test2 <- npn_leaf_lag %>% 
  filter(year == 2016 & station == "1afdaa42-0a1f-4610-b9cd-c30a1493a2c9")
  
ggplot() +
  geom_point(data = test1, aes(x = doy, y = percentage, color = "leaf"), linewidth = 1.5, alpha = 0.75) +
  geom_point(data = test2, aes(x = doy, y = percentage, color = "leaf_lag"), linewidth = 0.5, alpha = 0.75) 


to_plot_station <- to_plot %>% 
  filter(station == stationlist[i]) %>% 
  mutate(pollen = replace_na(pollen, 0)) 

to_plot %>% 
  filter(year == 2022 & station == "5effe609-c645-4620-bc99-a3b34934897c") %>% 
  ggplot() +
  geom_line(aes(x = doy, y = leaf, color = "leaf"), linewidth = 1.5, alpha = 0.75) +
  geom_line(aes(x = doy, y = leaf_lag, color = "leaf_lag"), linewidth = 0.5, alpha = 0.75) 









# check the species outlier

species_obs <- npn_wind %>% 
  filter(species_id==297 & phenophase_status == 1) 

ggplot(species_obs) +
  geom_histogram(aes(x = doy))

x_den<-density(species_obs$doy, from = 0, to = 364)
x_thres <- data.frame(x = x_den$x, y = x_den$y) %>%
  arrange(y) %>%
  head(1) %>%
  pull(x)

# show the detail of the flower and leaf data & the effect of the lag

sample_staiton <- points_within_buffer_filtered[["5effe609-c645-4620-bc99-a3b34934897c"]]

test <- sample_staiton %>% 
  group_by(species_id,year) %>% 
  summarise(count = n())

sample_staiton_uni <- sample_staiton %>% 

  mutate(individual_id = as.factor(individual_id),
         species_id =  as.factor(species_id))

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

