library(nimble)
library(coda)

posterior <- list()

for (i in 1:23) {
  # Extract species name
  species_name <- speciesoi$latin_name[i]
  # Print species name
  print(species_name)
  # Run spatial model
  spatial <- spatial_model(species_name)
  # Run temporal model
  temporal <- temporal_model(species_name)
  
  # Store spatial and temporal results in a list
  species_data <- list(
    Temporal = temporal,
    Spatial = spatial
  )
  
  # Add species data to posterior list with species name as key
  posterior[[species_name]] <- species_data
}


write_rds(posterior, "posterior.rds")



pdf("vignette/all_plots_bayesian_new.pdf")
# Loop through unique latin_names and create a plot for each
for (ln in speciesoi$latin_name) {
  plot_data <- anomaly_data %>%
    filter(latin_name == ln)
  
  p <- ggplot(data = plot_data, aes(x = spring_avg_temp, y = leaf)) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "black") +
    geom_point(aes(color = as.factor(individual_id))) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, aes(color = as.factor(individual_id), fill = as.factor(individual_id))) +
    labs(title = ln) +
    theme(legend.position = "none")  # Remove the entire legend
  
  p2 <- ggplot() +
    geom_density(data = data.frame(Temporal = posterior[[ln]]$Temporal), aes(x = Temporal, fill = "Temporal"), color = "blue", alpha = 0.5) +
    geom_density(data = data.frame(Spatial = posterior[[ln]]$Spatial), aes(x = Spatial, fill = "Spatial"), color = "red", alpha = 0.5) +
    xlab("Sensitivity (dDay/dT)") +
    ylab("Density") +
    scale_fill_manual(values = c("Temporal" = "blue", "Spatial" = "red"))
  
  print(p+p2)
}
# Close the PDF file
dev.off()

