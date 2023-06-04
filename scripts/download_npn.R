library(tidyverse)
library(rnpn)

# download data
v_taxa <- c("Quercus", "Cupressaceae", "Ambrosia", "Morus", "Pinaceae", "Ulmus", "Fraxinus", "Betula", "Poaceae", "Acer", "Populus")

path_npn <- "/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/climate/"
# npn_phenophases <- rnpn::npn_phenophases()
npn_species <- rnpn::npn_species()

for (taxaoi_short in v_taxa) {

  spid <- npn_species %>%
    filter(genus == taxaoi_short | family_name == taxaoi_short) %>%
    pull(species_id)
  
  npn <- npn_download_individual_phenometrics(
  request_source = 'LY',
  species_ids = spid,
  years = c(2000:2023), # years to include
  pheno_class_ids = c(1, 2, 3, 6,7,8),
  additional_fields = c("ObservedBy_Person_ID","Observed_Status_Conflict_Flag","multiple_firsty","partner_group","dataset_id"),
  climate_data = TRUE
)
  write_rds(npn, str_c(path_npn, taxaoi_short, ".rds"))
}

# npn <- read_rds(str_c(path_npn, "Quercus", ".rds"))
