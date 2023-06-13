# Load required packages

if (!exists("data")) {
  source("scripts/function_npn_select_model_data.R")
  data <- get_modelled_data()
}
# Read the TIFF file
raster_data <- terra::rast("/nfs/turbo/seas-zhukai/climate/CHELSA/climatology/bio1.tif")

# plot(raster_data)
library(patchwork)
library(MASS)

# Define a function to generate the plot
generate_plot <- function(df, variable, title) {
  df %>%
    mutate(tem = terra::extract(raster_data, cbind(lon, lat))) %>% 
    ggplot() +
    geom_point(aes(x = tem$bio1, y = {{ variable }})) +
    geom_smooth(method = "rlm", aes(x = tem$bio1, y = {{ variable }}), se = FALSE) +
    ggtitle(title)
}

# Generate plots for laggdd
site_gg_laggdd <- map2(data, names(data), ~generate_plot(.x, laggdd, .y))
combined_plot_gdd <- wrap_plots(site_gg_laggdd, nrow = 4, ncol = 2)

# Generate plots for lag
site_gg_lagday <- map2(data, names(data), ~generate_plot(.x, lag, .y))
combined_plot_day <- wrap_plots(site_gg_lagday, nrow = 4, ncol = 2)
