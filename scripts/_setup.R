# load packages
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(dplyr, 
               tidyr, 
               lubridate, 
               rnpn, 
               sf, 
               geosphere, 
               prism, 
               readr, 
               purrr, 
               ggplot2, 
               imputeTS, 
               zoo, 
               ggpmisc, 
               viridis, 
               ggthemes, 
               ggpubr)

# set parameters
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  eval = TRUE,
  warning = FALSE
)

# link paths
if (!dir.exists("data")) { # create symlink; final release needs to copy relevant files
  system("ln -s E:\\phenology data")
}

# data path tags are
# - `prism`: climate data prism
# - `npn`: national phenology network data
# - `nab`: national allergy bureau

.path <- list( # hidden variable won't be removed
  npn = "data/NPN_220620.csv",
  nab = "data/NAB_data_request_220308e.csv",
  prism = "data/prism",
  
  npn_clean = "data/NPN_clean.csv",
  nab_clean = "data/NAB_clean.csv",
  
  npn_clean_tem = "data/NPN_clean_tem.csv",
  nab_clean_tem = "data/NAB_clean_tem.csv",
  
  npn_extract = "scripts/function-extract-prism-npn.R",
  nab_extract = "scripts/function-extract-prism-nab.R",
  
  
  table_s1 = "final/table_s1.csv"
  #npn_scaled_near_nab = "data/NPN_near_NAB_scaled_220718.csv",
)