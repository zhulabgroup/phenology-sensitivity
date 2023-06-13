
compare_figure_1 <- ggplot(combined_table_lag) +
    geom_point(aes(x = lagday_std_normal, y = laggdd_std_normal)) +
    geom_abline(intercept = 0, slope = 1, color = "red")

compare_figure_2 <- combined_table_lag %>% 
  mutate(difference = lagday_std_normal-laggdd_std_normal) %>% 
  ggplot(aes(x = reorder(species_id, difference), y = difference)) +
  geom_col() +
  coord_flip() +
  ylab("day - gdd") +
  xlab("species")
  
compare_figure <- compare_figure_1+compare_figure_2