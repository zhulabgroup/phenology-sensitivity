# tree--------

tree$tip.label <- tree$tip.label %>% gsub("_", " ", .)

diff_color <- data.frame(
  species = species_sensitivity_PMM$species,
  diff = species_sensitivity_PMM$mean_b_spatial ##################
) 
# Assuming `tree` is a phylogenetic tree object and `lam_bc_pmm` contains the lambda values for the leaves
# You can use the `phytools` package to compute ancestral node values by averaging over descendant leaves.



# Step 1: Create a named vector for the lambda values (`lam_bc_pmm`), with species names as names
lambda_tips <- setNames(diff_color$diff, tree$tip.label) # Assuming `lam_bc_pmm` contains species' lambda values

# Step 2: Use `fastAnc` to calculate ancestral states (lambda values) for internal nodes
lambda_nodes <- fastAnc(tree, diff_color$diff) # Computes the ancestral values by averaging the lambda values of descendants

# Step 3: Combine tip and node lambda values into one vector for plotting
lambda_all <- c(lambda_tips, lambda_nodes)

# Step 4: Create the ggtree plot and color the tips and nodes based on their lambda values
tree_plot <- ggtree(tree, aes(color = lambda_all)) +  # Shrink branch lengths with xscale
  geom_tree() + 
  scale_color_gradient2(low = "blue", mid = "black", high = "red", midpoint = 0, 
                        name = "Spatial sensitivity,\ndays/°C)") +  # Darker color gradient
  theme_tree()+
  theme(
    plot.margin = margin(t = 5, r = 0, b = 30, l = 0),
    legend.position = c(0.05, 0.95),  # Position legend inside plot area
    legend.justification = c(0, 1),   # Align legend to top-left
    legend.direction = "horizontal",    # Display legend vertically
    legend.background = element_rect(fill = "white", color = NA),  # Optional: add background
    legend.title = element_text(size = 10),  # Adjust title size if needed
    legend.key = element_rect(color = NA),   # Remove key borders
    legend.margin = margin(0, 0, 0, 0)       # Remove legend margins
  ) +
  guides(color = guide_colorbar(title.position = "top", title.hjust = 0.5, label.position = "bottom"))



# PMM
tip_order <- tree_plot$data %>% 
  filter(isTip) %>% 
  arrange(y) %>% 
  pull(label)

# Use this order to reorder your species in the second figure
species_sensitivity_PMM$species <- factor(species_sensitivity_PMM$species, levels = tip_order)


sensitivity_plot <- species_sensitivity_PMM %>% 
  ggplot(aes(x = mean_b_spatial, y = species, color = mean_b_spatial)) +  #################
geom_point(shape = 20, size = 2, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = `2.5%_b_spatial`, xmax = `97.5%_b_spatial`), width = 0.2) + # Removed redundant color ########
scale_color_gradient2(
  low = "blue", mid = "black", high = "red", midpoint = 0, 
  name = "Spatial sensitivity,\ndays/°C)"
) +  
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = NULL, # Clearer to use NULL for an empty title
    y = NULL,     # Clearer to use NULL for an empty y-axis label
    x = "Spatial sensitivity, days/°C"
  ) +
  xlim(-10, 10) +  # Optional: adjust if needed
  # facet_grid(genus ~ ., scales = "free_y", space = "free_y") + # Optional facet
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(face = "italic"), # Ensure italics for species names
    legend.text = element_text() # Use element_text instead of element_markdown
  )


# HMM
species_sensitivity_HMM$species <- factor(species_sensitivity_HMM$species, levels = tip_order)


HMM_sensitivity_plot <- species_sensitivity_HMM %>% 
  ggplot(aes(x = mean_b_spatial, y = species, color = mean_b_spatial)) +  #################
geom_point(shape = 20, size = 2, position = position_dodge(width = 0.5), color = "black") +
  geom_errorbar(aes(xmin = `2.5%_b_spatial`, xmax = `97.5%_b_spatial`), width = 0.2, color = "black") + # Removed redundant color ########
geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = NULL, # Clearer to use NULL for an empty title
    y = NULL,     # Clearer to use NULL for an empty y-axis label
    x = "Spatial sensitivity days/°C"
  ) +
  xlim(-10, 10) +  # Optional: adjust if needed
  # facet_grid(genus ~ ., scales = "free_y", space = "free_y") + # Optional facet
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    legend.text = element_text() # Use element_text instead of element_markdown
  )