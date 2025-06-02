library(devtools)
devtools::install_github("jinyizju/V.PhyloMaker2")

joint_data <- read.csv("species_summary.csv")

species_list <- joint_data %>%
  distinct(species, genus, family)

library(V.PhyloMaker2)

result <- phylo.maker(species_list, 
                      tree = GBOTB.extended.TPL, 
                      nodes = nodes.info.1.TPL, 
                      scenarios = c("S1", "S2", "S3"))

write.tree(result[["scenario.1"]], "../data/phylogenetics/PhyloMaker_tree_scenario1.nwk")