ggplot() +
  geom_density(data = data.frame(b = temporal), aes(x = b, fill = "Temporal"), color = "blue", alpha = 0.5) +
  geom_density(data = data.frame(b = spatial), aes(x = b, fill = "Spatial"), color = "red", alpha = 0.5) +
  xlab("Sensitivity (dDay/dT)") +
  ylab("Density") +
  scale_fill_manual(values = c("Temporal" = "blue", "Spatial" = "red"), 
                    labels = c("Temporal", "Spatial"))


posterior <- read_rds("/Volumes/seas-zhukai/phenology/phenology_leaf_flower_lag/posterior.rds")

species <- list(
  Temporal = temporal,
  Spatial = spatial
)


posterior$"Acer rubrum" = species


write_rds(posterior, "/Volumes/seas-zhukai/phenology/phenology_leaf_flower_lag/posterior.rds")
