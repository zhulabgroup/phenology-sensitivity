library(tidyverse)
source("scripts/function_get_bufferednpn_percentage.R")
npn_leaf <- get_buffle_npn_percentage(50,1,"Quercus",10)
npn_flower <- get_buffle_npn_percentage(50,7,"Quercus",10)
source("scripts/function_get_smoothed_nab_for_taxa.R")
nab <- get_smoothed_nab("Quercus")

# stardarize the data (should be put into getting data?)
npn_leaf_s <- npn_leaf %>% 
  select(station,year, doy,count_w) %>% 
  rename(leaf = count_w) %>% 
  mutate(doy = doy+15) %>% 
  mutate(doy = ifelse(doy > 365, doy - 365, doy))


npn_flower_s <- npn_flower %>% 
  select(station,year, doy,count_w) %>% 
  rename(flower = count_w)

nab_s <- nab %>% 
  mutate(doy = lubridate::yday(date)) %>% 
  select(stationid, year, doy, count_w) %>% 
  rename(station = stationid, pollen = count_w) %>% 
  group_by(station, year) %>% 
  mutate(pollen = pollen/max(pollen)) %>% 
  ungroup()


# visial check
to_plot <- full_join(npn_leaf_s,npn_flower_s,by = c("station","year","doy"))  %>% 
  left_join(nab_s,by = c("station","year","doy")) %>% 
  group_by(station,year) %>%
  filter(!all(is.na(pollen)))

stationlist <- unique(to_plot$station)

site_gg <- vector(mode = "list")
correlation_table <- tibble(year = integer(), leaf_rmse = numeric(), flower_rmse = numeric(), station = character())

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

for (i in seq_along(stationlist)) {
  to_plot_station <- to_plot %>% 
    filter(station == stationlist[i]) %>% 
    mutate(pollen = replace_na(pollen, 0)) 

  # Use RMSE in calculation
  cor_data <- to_plot_station %>%
    group_by(year) %>%
    summarize(leaf_rmse = rmse(leaf, pollen),
              flower_rmse = rmse(flower, pollen)) %>% 
    mutate(station = stationlist[i])
  
  correlation_table <- bind_rows(correlation_table, cor_data)
  
  site_gg[[i]] <-  to_plot_station %>% 
    ggplot() +
    geom_line(aes(x = doy, y = pollen, color = "pollen"), linewidth = 2, alpha = 0.75) +
    geom_line(aes(x = doy, y = leaf, color = "leaf"), linewidth = 1.5, alpha = 0.75) +
    geom_line(aes(x = doy, y = flower, color = "flower"), linewidth = 1, alpha = 0.75) +
    facet_wrap(~ year)+
    geom_text(data = cor_data, 
              aes(x = Inf, y = Inf, 
                  label = paste0("leaf_rmse: ", round(leaf_rmse, 2), "\n", 
                                 "flower_rmse: ", round(flower_rmse, 2))), 
              hjust = 1, vjust = 1, size = 4) +
    ggtitle(stationlist[i])
}


pdf("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/lag_RMSE_Smooth_Quercus_leafflowerpollen.pdf", width = 8, height = 8 * .618)
print(site_gg)
dev.off()

correlation_table <- correlation_table %>% select(-leaf_correlation,-flower_correlation)

write_rds(site_gg,"/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/lag_RMSE_Smooth_Quercus_leafflowerpollen.rds")
write_csv(correlation_table,"/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/lag_RMSE_Smooth_Quercus_leafflowerpollen.csv")

  