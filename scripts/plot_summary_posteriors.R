##### summary figure
posteriors <- readRDS("/Volumes/seas-zhukai/phenology/phenology_leaf_flower_lag/posterior.rds")

to_plot = data.frame()
for (i in 1:23) {
  temporal <- posteriors[[i]]$Temporal %>%     
    as.data.frame() %>%
    setNames("sample") %>%
    filter(row_number() %% 10 == 0) %>% 
    mutate(dim = "T", species = names(posteriors)[i]) 
  
  spatial <- posteriors[[i]]$Spatial %>% 
    as.data.frame() %>%
    setNames("sample") %>%
    filter(row_number() %% 10 == 0) %>% 
    mutate(dim = "S", species = names(posteriors)[i]) 
  
  to_plot = rbind(to_plot,temporal,spatial)
}

library(ggridges)

ggplot(to_plot, aes(x = sample, y = species, height = after_stat(density), group = interaction(species, dim), fill = dim)) +
  geom_density_ridges(alpha = 0.5, scale = .5) +
  scale_x_continuous(limits = c(-15, 5)) 


to_plot$species <- factor(to_plot$species, levels = rev(unique(to_plot$species)))

ggplot(to_plot, aes(x = sample, y = species, height = after_stat(density), group = interaction(species, dim), fill = dim)) +
  geom_density_ridges(alpha = 0.5, scale = .5) +
  scale_x_continuous(limits = c(-15, 5)) +
  theme_minimal()
