source("scripts/function_npn_select_model_data.R")

data <- get_modelled_data()

site_gg <- vector(mode = "list")

for (i in seq_along(data) ) {
  histograms <- ggplot(data[[i]], aes(x = lag)) +
    geom_histogram(binwidth = 1) +
    facet_wrap(~ species_id, scales = "free")
  
  site_gg <- c(site_gg, list(histograms))         
}

pdf("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/lag_histogram.pdf", width = 8, height = 8 * .618)
print(site_gg) # this is your list
dev.off()