species_stat <- read_csv("data/figure1_stat.csv") %>% 
  filter(is.na(scientific_name))

# Find the 9 points with the highest n values
top_n_points <- head(species_stat[order(species_stat$n, decreasing = TRUE), ], 9)

observation_slope <- ggplot(species_stat, aes(x = n, y = slope)) +
  geom_point() +
  labs(title = "Scatter Plot of n vs. Slope", x = "n", y = "Slope") +
  ggrepel::geom_text_repel(
    data = top_n_points,
    aes(label = common_name),
    hjust = 1, vjust = 1, # Adjust text position
    box.padding = 0.5, point.padding = 0.5 # Add padding to labels
  ) 
