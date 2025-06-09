# load packages
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(dplyr, 
               tidyr, 
               lubridate, 
               prism, 
               readr, 
               purrr)
pacman::p_load_gh("jinyizju/V.PhyloMaker2") 

# link paths
if (!dir.exists("data")) { # create symlink; final release needs to copy relevant files
  system("ln -s /Volumes/seas-zhukai/proj-phenology-sensitivity/to_release data")
}

# data path tags are
# - `prism`: climate data prism
# - `npn`: national phenology network data
# - `herb`: herbarium data

.path <- list( # hidden variable won't be removed

  herb_meta = "data/Herb/meta_data.csv",
  herb_pheno = "data/Herb/phenology.csv",
  herb_flower = "data/Herb/herb_flower.csv",
  
  npn = "data/NPN/",
  npn_flower = "data/NPN/npn_flower.csv",
  
  prism_norm = "data/prism/complete_period_springmean.tif",
  prism_anom = "data/prism/",
  
  temperature_data = "data/temperature_data.csv",
  taxa_info = "data/taxa_info.csv",
  byspecies_summary = "data/species_summary.csv",
  
  model_hmm = "../scripts/PhenoPhyloMM_HMM_Yi.stan",
  model_pmm = "../scripts/PhenoPhyloMM_PMM_Yi.stan",
  tree = "data/PhyloMaker_tree_scenario1_total.nwk",
  
  sample_hmm = "data/sample_HMM.rds",
  sample_pmm = "data/sample_PMM.rds"
)