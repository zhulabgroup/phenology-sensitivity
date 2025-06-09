
library(ape)

tree <- read.tree(.path$tree) 


tree_species <- rownames(vcv(tree, corr = TRUE)) %>% 
  gsub("_", " ", .) 

# Create a data frame to match species in the tree with IDs
phylo_species_id <- data.frame(species = tree_species, sppid = seq_along(tree_species))

# Join the species IDs from the tree with the temperature data
temperature_data_model_phylo <- read.csv(.path$temperature_data)  %>% 
  dplyr::select(species, doy, norm, anom, yeart) %>% 
  right_join(phylo_species_id, by = "species")  # Join based on species names

nspecies <- n_distinct(temperature_data_model_phylo$species)

# HMM ----------
library(rstan)
options(mc.cores = parallel::detectCores())

fitlamb0 <- stan(.path$model_hmm,
                 data = list(N = nrow(temperature_data_model_phylo),
                             n_sp = nspecies,
                             sp = temperature_data_model_phylo$sppid,
                             x1 = temperature_data_model_phylo$norm,
                             x2 = temperature_data_model_phylo$anom,
                             y = temperature_data_model_phylo$doy,
                             Vphy = vcv(tree, corr = TRUE)),
                 iter = 4000, #4000
                 warmup = 2000, # half the iter as warmup is default, but leaving in case we want to change
                 chains = 4, #4
                 seed = 2
)
saveRDS(fitlamb0, .path$sample_hmm)

# PMM ----------
# 
fitlambest <- stan(.path$model_pmm,
                   data=list(N=nrow(temperature_data_model_phylo),
                             n_sp=nspecies,
                             sp=temperature_data_model_phylo$sppid,
                             x1=temperature_data_model_phylo$norm,
                             x2=temperature_data_model_phylo$anom,
                             y=temperature_data_model_phylo$doy,
                             Vphy=vcv(tree, corr = TRUE)), # vcv: Phylogenetic Variance-covariance or Correlation Matrix

                   iter = 8000, #4000
                   warmup = 4000, # half the iter as warmp is default, but leaving in case we want to change
                   chains = 4, #4
                   seed = 2
)

## Save fitted posterior
saveRDS(fitlambest, .path$sample_pmm)