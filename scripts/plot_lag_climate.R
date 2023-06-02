# Load required packages

# Read the TIFF file
raster_data <- terra::rast("/nfs/turbo/seas-zhukai/climate/CHELSA/climatology/bio1.tif")

# plot(raster_data)

source("scripts/npn_select+model_data.R")

species_data <- get_modelled_data()

site_gg <- vector(mode = "list")

for (i in seq_along(species_data) ) {
  
  models <- species_data[[i]] %>% 
    mutate(tem = terra::extract(raster_data, cbind(lon, lat))) %>% 
    ggplot() +
    geom_point(aes(x = tem$bio1, y = lag))+
    geom_smooth(method = "rlm",aes(x = tem$bio1, y = lag),se = FALSE) +
    ggtitle(names(species_data[i]))
    
  site_gg <- c(site_gg, list(models))         

}

pdf("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/lag.pdf", width = 8, height = 8 * .618)
print(site_gg) # this is your list
dev.off()