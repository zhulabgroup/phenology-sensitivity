if (!exists("points_within_buffer")) {
  source("scripts/function_generate_nab_npn_buffer.R")
  points_within_buffer <- get_buffle_data(200)
}

# prepare acer nab data

nab_acer <- read_rds("/nfs/turbo/seas-zhukai/phenology/nab/clean/2023-04-25/nab_renew.rds") %>% filter(taxa=="Acer")

library(lubridate)

site_gg <- vector(mode = "list")

for (i in seq_along(points_within_buffer) ) {
  
  id <- names(points_within_buffer[i])
  
  sample_nab <- nab_acer %>% filter(stationid==id)
  
  site_gg[[i]] <- ggplot() + 
    geom_tile(data = sample_nab, aes(x = yday(date), y = year(date), fill = ifelse(count == 0, 0, log(count)))) + # loged
    scale_fill_gradient(low = "white", high = "brown") +
    labs(x = "Day of year", y = "Year") +
    geom_point(data = points_within_buffer[[i]], aes(x = first_yes_doy, y = first_yes_year)) +
    ggtitle(id)
}

pdf("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/leaf_pollen_200.pdf", width = 8, height = 8 * .618)
print(site_gg)
dev.off()

