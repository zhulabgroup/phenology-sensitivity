joint_data <- read.csv("species_summary.csv")  %>% 
  dplyr::select(yeart, norm, anom, doy, species, genus, dataset)

# HMM ----------
library(rstan)
options(mc.cores = parallel::detectCores())

fitlamb0 <- stan("../data/phylogenetics/PhenoPhyloMM_HMM_Yi.stan",
                 data = list(N = nrow(temperature_data_model_phylo),
                             n_sp = nspecies,
                             sp = temperature_data_model_phylo$sppid,
                             x1 = temperature_data_model_phylo$yeart,
                             x2 = temperature_data_model_phylo$anom,
                             y = temperature_data_model_phylo$doy,
                             Vphy = vcv(tree, corr = TRUE)),
                 iter = 4000, #4000
                 warmup = 2000, # half the iter as warmup is default, but leaving in case we want to change
                 chains = 4, #4
                 seed = 2
)
saveRDS(fitlamb0, "../data/phylogenetics/fit_model_HMM_all.rds")

# PMM ----------
tree <- read.tree("../data/phylogenetics/PhyloMaker_tree_scenario1.nwk") 

fitlambest <- stan("../data/phylogenetics/PhenoPhyloMM_PMM_Yi.stan",
                   data=list(N=nrow(temperature_data_model_phylo),
                             n_sp=nspecies,
                             sp=temperature_data_model_phylo$sppid,
                             x1=temperature_data_model_phylo$yeart,
                             x2=temperature_data_model_phylo$anom,
                             y=temperature_data_model_phylo$doy,
                             Vphy=vcv(tree, corr = TRUE)), # vcv: Phylogenetic Variance-covariance or Correlation Matrix
                   
                   iter = 8000, #4000
                   warmup = 4000, # half the iter as warmp is default, but leaving in case we want to change
                   chains = 4, #4
                   seed = 2 
)

## Save fitted posterior
saveRDS(fitlambest, "../data/phylogenetics/fit_model_PMM_all.rds")