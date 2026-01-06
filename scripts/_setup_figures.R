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
               ggside,
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
  system("ln -s /Volumes/seas-zhukai/proj-phenology-sensitivity/review_np ../data")
}


.path <- list( # hidden variable won't be removed

  temperature_data = "../data/Feb_Mar/temperature_data.csv",
  taxa_info = "../data/taxa_info.csv",
  byspecies_summary = "../data/Feb_Mar/species_summary.csv",

  tree = "../data/PhyloMaker_tree_scenario1_total.nwk",
  
  sample_hmm = "../data/Feb_Mar/sample_HMM.rds",
  sample_pmm = "../data/Feb_Mar/sample_PMM.rds"
  )