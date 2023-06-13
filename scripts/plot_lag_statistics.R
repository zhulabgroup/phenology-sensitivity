library(patchwork)

plot1 <- combined_table_lag %>% 
ggplot(aes(lagday_average, as.factor(species_id))) +
  geom_errorbar(aes(xmin = lagday_average - lagday_std_dev, xmax = lagday_average + lagday_std_dev),
                width = 0.2,                      # Width of the error bars
                color = "blue",                    # Color of the error bars
                size = 1.5)   +                     # Size of the error bars
  geom_point() 

plot2 <- combined_table_lag %>% 
  ggplot(aes(laggdd_average, as.factor(species_id))) +
  geom_errorbar(aes(xmin = laggdd_average - laggdd_std_dev, xmax = laggdd_average + laggdd_std_dev),
                width = 0.2,                      # Width of the error bars
                color = "blue",                    # Color of the error bars
                size = 1.5)   +                     # Size of the error bars
  geom_point() 

combinefigure <- plot1 + plot2








# delete_grass <- data[setdiff(names(data), "Poaceae")]
# 
# delete_grass_tibble <- map_dfr(names(delete_grass), ~ tibble(name = .x, value = delete_grass[[.x]]))
# 
# delete_grass_tibble %>% 
# ggplot()+
#   geom_point(aes(x = value$lag,y = as.factor(value$species_id)))