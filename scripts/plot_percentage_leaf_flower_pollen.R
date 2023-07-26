source("scripts/function_get_lagbufferednpn_percentage.R")
npn_leaf <- get_buffle_npn_percentage(50,1,"Quercus",10)[[1]]
npn_leaf_lag <- get_buffle_npn_percentage(50,1,"Quercus",10,TRUE)[[1]]
npn_flower <- get_buffle_npn_percentage(50,7,"Quercus",10)[[1]]
source("scripts/function_get_smoothed_nab_for_taxa.R")
nab <- get_smoothed_nab("Quercus")

# stardarize the data (should be put into getting data?)
standardize_data <- function(data, new_name) {
  renamed_data <- data %>% 
    select(station, year, doy, count_w) %>% 
    rename({{new_name}} := count_w)
  
  unified_data <- renamed_data %>% 
    group_by(station, year) %>% 
    summarise(total = sum({{new_name}})) %>%
    inner_join(renamed_data, by = c("station","year")) %>% 
    mutate({{new_name}} := {{new_name}} / total) %>% 
    select(-total) %>% 
    ungroup()

  
  return(unified_data)
}

npn_leaf_s <- standardize_data(npn_leaf, leaf)
npn_leaf_lag_s <- standardize_data(npn_leaf_lag, leaf_lag) 
npn_flower_s <- standardize_data(npn_flower, flower)
nab_s <- standardize_data(nab %>% 
                            mutate(doy = lubridate::yday(date))%>% 
                            rename(station = stationid), pollen)
to_plot <- npn_leaf_s %>% 
  full_join(npn_leaf_lag_s, by = c("station", "year", "doy")) %>% 
  full_join(npn_flower_s, by = c("station", "year", "doy")) %>% 
  left_join(nab_s,by = c("station","year","doy")) %>% 
  group_by(station,year) %>%
  filter(!all(is.na(pollen)))

stationlist <- unique(to_plot$station)

site_gg <- vector(mode = "list")

correlation_table <- tibble(year = integer(), leaf_rmse = numeric(),leaf_lag_rmse = numeric(), flower_rmse = numeric(), station = character())

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2/max(actual) - min(actual), na.rm = TRUE))
}


for (i in seq_along(stationlist)) {
  
  
  to_plot_station <- to_plot %>% 
    filter(station == stationlist[i]) %>% 
    mutate(pollen = replace_na(pollen, 0)) 
  
  cor_data <- to_plot_station %>%
    group_by(year) %>%
    summarize(leaf_rmse = rmse(leaf, pollen),
              leaf_lag_rmse = rmse(leaf_lag, pollen),
              flower_rmse = rmse(flower, pollen)) %>% 
    mutate(station = stationlist[i])
  
  correlation_table <- bind_rows(correlation_table, cor_data)

  
  site_gg[[i]] <-  to_plot_station %>% 
    ggplot() +
    geom_line(aes(x = doy, y = pollen, color = "pollen"), linewidth = 1, alpha = 0.75) +
    geom_line(aes(x = doy, y = leaf, color = "leaf"), linewidth = 1.5, alpha = 0.75) +
    geom_line(aes(x = doy, y = flower, color = "flower"), linewidth = 1, alpha = 0.75) +
    geom_line(aes(x = doy, y = leaf_lag, color = "leaf_lag"), linewidth = 0.5, alpha = 0.75) +
    facet_wrap(~ year)+
    geom_text(data = cor_data, 
              aes(x = Inf, y = Inf, 
                  label = paste0("leaf_rmse: ", round(leaf_rmse, 2), "\n",
                                 "leaf_lag_rmse: ", round(leaf_lag_rmse, 2), "\n",
                                 "flower_rmse: ", round(flower_rmse, 2))), 
              hjust = 1, vjust = 1, size = 4) +
    ggtitle(stationlist[i])
}


pdf("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/lag_RMSE_Smooth_Quercus_leafflowerpollen.pdf", width = 8, height = 8 * .618)
print(site_gg)
dev.off()


write_rds(site_gg,"/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/lag_RMSE_Smooth_Quercus_leafflowerpollen.rds")
write_csv(correlation_table,"/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/lag_RMSE_Smooth_Quercus_leafflowerpollen.csv")

  