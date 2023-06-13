# Load required packages

# Read the TIFF file
raster_data <- terra::rast("/nfs/turbo/seas-zhukai/climate/CHELSA/climatology/bio1.tif")

# plot(raster_data)
library(patchwork)

site_gg <- vector(mode = "list")

for (i in seq_along(species_data) ) {
  
  models <- species_data[[i]] %>% 
    mutate(tem = terra::extract(raster_data, cbind(lon, lat))) %>% 
    ggplot() +
    geom_point(aes(x = tem$bio1, y = laggdd))+
    geom_smooth(method = "rlm",aes(x = tem$bio1, y = laggdd),se = FALSE) +
    ggtitle(names(species_data[i]))
    
  site_gg <- c(site_gg, list(models))         

}

combined_plot_gdd <- wrap_plots(site_gg, nrow = 4, ncol = 2)

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

combined_plot_gdd <- wrap_plots(site_gg, nrow = 4, ncol = 2)
