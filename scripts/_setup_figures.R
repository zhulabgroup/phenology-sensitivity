# load packages
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(dplyr, 
               tidyr, 
               lubridate, 
               readr, 
               purrr,
               ggplot2,
               patchwork,
               sf,
               ggtext,
               ape,
               phytools,
               rstan,
               ggtree,
               stringr,
               ggdist,
               gghalves,
               raster)


# code that might need to install ggtree:
#   if (!requireNamespace("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# BiocManager::install("ggtree")


# set theme
my_theme <- function() {
  theme_classic(base_size = 14) +
    theme(
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      #plot.title = element_text(size = 18, face = "bold"),
      #plot.subtitle = element_text(size = 16),
      #legend.title = element_text(size = 14, face = "bold"),
      #legend.text = element_text(size = 12)
    )
}

theme_set(my_theme())

# link paths
if (!dir.exists("data")) { # create symlink; final release needs to copy relevant files
  system("ln -s /Volumes/seas-zhukai/proj-phenology-sensitivity/to_release data")
}


.path <- list( # hidden variable won't be removed
  norm_anom = "data/prism/yearly_anomaly_normality.csv",
  prism_norm = "data/prism/complete_period_springmean.tif",
  
  temperature_data = "data/temperature_data.csv",
  taxa_info = "data/taxa_info.csv",
  byspecies_summary = "data/species_summary.csv",

  tree = "data/PhyloMaker_tree_scenario1_total.nwk",
  
  sample_hmm = "data/sample_HMM.rds",
  sample_pmm = "data/sample_PMM.rds"
  )