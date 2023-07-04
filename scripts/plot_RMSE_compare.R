rmse_table <- read_csv("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/RMSE_Smooth_Quercus_leafflowerpollen.csv") 


rmse_table_long <- rmse_table%>% 
  pivot_longer(cols = c("flower_rmse", "leaf_rmse"), names_to = "pheno", values_to = "rmse")


count_data <- rmse_table_long %>%
  group_by(pheno) %>%
  summarise(n = sum(!is.na(rmse)))

            
p <- rmse_table_long %>%
  ggplot(aes(x = pheno, y = rmse)) +
  geom_violinhalf(position = position_nudge(x = .2, y = 0)) +
  geom_jitter(alpha = 0.1, width = 0.15) +
  geom_text(data = count_data, aes(label = n, y = 0.3), vjust = -0.5, size = 5)

zero_compare <- rmse_table %>% 
  mutate(diff = leaf_rmse - flower_rmse) %>% 
  filter(!is.na(diff)) %>% 
  summarise(positive = sum(diff > 0), negative = sum(diff < 0))

label_df <- data.frame(label = "Flower better: 85, Leaf better: 37", x = 1, y = 0.3)

p1 <- rmse_table %>% 
  mutate(diff = leaf_rmse-flower_rmse) %>% 
  ggplot(aes(x = "Quercus", y = diff)) +
  geom_violinhalf(position = position_nudge(x = .2, y = 0)) +
  geom_jitter(alpha = 0.1, width = 0.15) +
  geom_hline(yintercept = 0, color = "red") +
  xlab("")+
  ylab("leaf_rmse-flower_rmse") +
  geom_text(data = label_df, aes(x = x, y = y, label = label), size = 5)


RMSE_violin <- p + p1 +
  patchwork::plot_annotation(tag_levels = "A") 


