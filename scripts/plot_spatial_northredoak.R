
quercus <- read_rds("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/Quercus_winsprtem.rds")

individual_aggre <- quercus %>%
  filter(common_name %in% c("northern red oak")) %>% 
  group_by(individual_id, longitude, latitude, common_name) %>%
  summarise(
    across(
      c(leaf, flower, lag, winter_avg_temp, spring_avg_temp),
      list(
        Mean = ~mean(.),
        Min = ~min(.)-mean(.),
        Max = ~max(.)-mean(.)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%   
  ungroup() %>% 
  mutate_at(vars(flower_Mean, lag_Mean, leaf_Mean, winter_avg_temp_Mean, spring_avg_temp_Mean), 
            ~ . - mean(.)) 


library(plotly)

gg <- ggplot(individual_aggre, aes(x = spring_avg_temp_Mean, y = leaf_Mean, text = spring_avg_temp_Min)) +
  geom_point() +
  geom_errorbarh(
    aes(
      xmin = spring_avg_temp_Mean + spring_avg_temp_Min,
      xmax = spring_avg_temp_Mean + spring_avg_temp_Max,
      y = leaf_Mean
    ),
    height = 0.1,  # Adjust as needed
    alpha = 0.2
  ) +
  geom_errorbar(
    aes(
      ymin = leaf_Mean + leaf_Min,
      ymax = leaf_Mean + leaf_Max,
      x = spring_avg_temp_Mean
    ),
    width = 0.1,  # Adjust as needed
    alpha = 0.2
  ) +
  geom_smooth(
    method = "lm", 
    formula = y ~ x - 1,  # Linear model with intercept forced to zero
    se = TRUE,           # Show uncertainty area (confidence interval)
    color = "blue"       # You can choose the color
  )+
  labs(
    x = "spring_avg_temp",
    y = "leaf"
  ) 
# title = "Scatter Plot with User-Defined Error Bars"

# Convert the ggplot object to a plotly object
ggplotly(gg)