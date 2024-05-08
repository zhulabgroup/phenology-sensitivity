

anomaly_data_neon <- anomaly_data %>% 
  filter(dataset_id=="'16'")

speciesoi <- as.data.frame(table(anomaly_data_neon$latin_name))

posterior <- list()
# 5-7 Quercus montana doesn't converge, I will rerun it
for (i in 1:7) {
  # Extract species name
  species_name <- speciesoi$Var1[i] %>% 
    as.character()
  # Print species name
  print(species_name)
  
  # Run temporal model
  temporal <- temporal_model(species_name)
  
  # Add species data to posterior list with species name as key
  posterior[[species_name]] <- temporal
}

write_rds(posterior, "posterior_neon.rds")

posterior_all <- read_rds("posterior.rds")
pdf("vignette/all_plots_bayesian_neon.pdf")
# Loop through unique latin_names and create a plot for each
for (ln in speciesoi$Var1) {
  plot_data <- anomaly_data_neon %>%
    filter(latin_name == ln)
  
  p <- ggplot(data = plot_data, aes(x = spring_avg_temp, y = leaf)) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "black") +
    geom_point(aes(color = as.factor(individual_id))) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, aes(color = as.factor(individual_id), fill = as.factor(individual_id))) +
    labs(title = ln) +
    theme(legend.position = "none")  # Remove the entire legend
  
  p2 <- ggplot() +
    geom_density(data = data.frame(Temporal = posterior[[ln]]), aes(x = Temporal, fill = "NEON"), color = "blue", alpha = 0.5) +
    # geom_density(data = data.frame(Temporal = posterior_all[[ln]]$Temporal), aes(x = Temporal, fill = "ALl"), color = "red", alpha = 0.5) +
    xlab("Sensitivity (dDay/dT)") +
    ylab("Density") +
    scale_fill_manual(values = c("Temporal" = "blue", "Spatial" = "red"))
  
  print(p+p2)
}
# Close the PDF file
dev.off()


posterior_all <- read_rds("posterior.rds")
posteriors <- readRDS("/Volumes/seas-zhukai/phenology/phenology_leaf_flower_lag/posterior.rds")


