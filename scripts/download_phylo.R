# library(devtools)
# devtools::install_github("jinyizju/V.PhyloMaker2")

# get taxa table------------
# Load and clean taxa_info
taxa_info <- read.csv(.path$herb_meta) %>%
  dplyr::select(species, genus, family) %>%
  distinct()

# Load npn_flower data
joint_data <- read.csv(.path$byspecies_summary) %>%
  distinct(species) %>%
  left_join(taxa_info %>% dplyr::select(species, genus) %>% distinct(), by = "species") %>% 
  left_join(taxa_info %>% dplyr::select(genus, family) %>% distinct(), by = "genus") %>% 
  mutate(taxa = ifelse(family %in% c("Cupressaceae", "Pinaceae"), family, genus))

write.csv(joint_data, .path$taxa_info, row.names = FALSE)


# download tree ----------
library(V.PhyloMaker2)

result <- phylo.maker(joint_data,
                      tree = GBOTB.extended.TPL,
                      nodes = nodes.info.1.TPL,
                      scenarios = c("S1", "S2", "S3"))

write.tree(result[["scenario.1"]], .path$tree)
