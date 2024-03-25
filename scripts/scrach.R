library(nimble)
library(coda)

posterior <- read_rds("posterior.rds")
write_rds(posterior, "/Volumes/seas-zhukai/phenology/phenology_leaf_flower_lag/posterior.rds")

posterior <- read_rds("posterior_neon.rds")
write_rds(posterior, "/Volumes/seas-zhukai/phenology/phenology_leaf_flower_lag/posterior_neon.rds")
posterior <- read_rds("/Volumes/seas-zhukai/phenology/phenology_leaf_flower_lag/posterior_neon.rds")

# Iterate over indices from 5 to 23
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



ggplot() +
  geom_density(data = data.frame(species_data$Temporal), aes(x = temporal, fill = "Temporal"), color = "blue", alpha = 0.5) +
  geom_density(data = data.frame(species_data$Spatial), aes(x = spatial, fill = "Spatial"), color = "red", alpha = 0.5) +
  xlab("Sensitivity (dDay/dT)") +
  ylab("Density") +
  scale_fill_manual(values = c("Temporal" = "blue", "Spatial" = "red"))



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



######## try thin
ggplot(data = posterior, aes(x = mu_b)) +
  geom_density(fill = "blue", alpha = 0.5) +
  xlab("b") +
  ylab("Density")

thinned_posterior <- posterior %>%
  filter(row_number() %% 500 == 0)

# Plot the thinned posterior
ggplot(data = thinned_posterior, aes(x = mu_b)) +
  geom_density(fill = "blue", alpha = 0.5) +
  xlab("b") +
  ylab("Density")

##### this part is for neon
path_npn <- "/Volumes/seas-zhukai/phenology/NPN/individual_phenometrics/leaf_flower/with_climate/"
npn <- read_rds(stringr::str_c(path_npn, "Quercus", ".rds"))
contingency_table <- as.data.frame(table(npn$partner_group, npn$dataset_id)) # 16 for neon data

anomaly_data_neon <- anomaly_data %>% 
  filter(dataset_id=="'16'")

speciesoi <- as.data.frame(table(anomaly_data_neon$latin_name))

posterior <- list()
# 5-7 Quercus montana doesn't converge, I will rerun it
for (i in 5:7) {
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

##### summary figure
# library
posteriors <- readRDS("~/Documents/phenology-npn-flower-leaf-lag/posterior.rds")

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
  scale_x_continuous(limits = c(-15, 5)) 