library(patchwork)

create_plot <- function(data, average_var, std_dev_var) {
  average <- sym(average_var)
  std_dev <- sym(std_dev_var)
  
  data %>%
    ggplot(aes(x = !!average, y = as.factor(species_id))) +
    geom_errorbar(aes(xmin = !!average - !!std_dev, 
                      xmax = !!average + !!std_dev),
                  width = 0.2, color = "blue", size = 1.5) +
    geom_point()
}

plot1 <- create_plot(combined_table_lag, "lagday_average", "lagday_std_dev")
plot2 <- create_plot(combined_table_lag, "laggdd_average", "laggdd_std_dev")

combinefigure <- plot1 + plot2








# delete_grass <- data[setdiff(names(data), "Poaceae")]
# 
# delete_grass_tibble <- map_dfr(names(delete_grass), ~ tibble(name = .x, value = delete_grass[[.x]]))
# 
# delete_grass_tibble %>% 
# ggplot()+
#   geom_point(aes(x = value$lag,y = as.factor(value$species_id)))