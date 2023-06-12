source("scripts/function_npn_select_model_data.R")

data <- get_modelled_data()

site_gg <- vector(mode = "list")

for (i in seq_along(data) ) {
  
  histograms <- data[[i]] %>% 
   ggplot() +
    geom_point(aes(x = yr,y = individual_id)) +
    facet_wrap(~ species_id, scales = "free")
  
  site_gg <- c(site_gg, list(histograms))         
}

pdf("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/data_availibility.pdf", width = 8, height = 8 * .618)
print(site_gg) # this is your list
dev.off()