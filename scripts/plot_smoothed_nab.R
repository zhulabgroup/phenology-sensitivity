library(tidyverse)

source("scripts/function_get_smoothed_nab_for_taxa.R")
nab_smooth <- get_smoothed_nab("Quercus")

stationlist <- unique(nab_smooth$stationid)

site_gg <- vector(mode = "list")

for (i in seq_along(stationlist)) {
  site_gg[[i]] <- nab_smooth %>% 
    filter(stationid==stationlist[i]) %>% 
    ggplot()  +
    geom_point(aes(x = lubridate::yday(date), y = count, color = "original"),size = 1, alpha = 0.5) +
    geom_line(aes(x = lubridate::yday(date), y = count_l, color = "linear")) +
    geom_line(aes(x = lubridate::yday(date), y = count_w, color = "whit"), linewidth = 1.5, alpha = 0.75) +
    facet_wrap(~ year)+
    ggtitle(stationlist[i])
}

pdf("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/Quercus_smooth.pdf", width = 8, height = 8 * .618)
print(site_gg)
dev.off()

# plot smoothed npn
stationlist <- unique(npn$station)

site_gg <- vector(mode = "list")

for (i in seq_along(stationlist)) {
  site_gg[[i]] <- npn %>% 
    filter(station==stationlist[i]) %>% 
    ggplot()  +
    geom_point(aes(x = doy, y = percentage, color = "original"),size = 1, alpha = 0.5) +
    geom_line(aes(x = doy, y = count_l, color = "linear")) +
    geom_line(aes(x = doy, y = count_w, color = "whit"), linewidth = 1.5, alpha = 0.75) +
    facet_wrap(~ year)+
    ggtitle(stationlist[i])
}

pdf("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/Quercus_smooth_npn_flower.pdf", width = 8, height = 8 * .618)
print(site_gg)
dev.off()