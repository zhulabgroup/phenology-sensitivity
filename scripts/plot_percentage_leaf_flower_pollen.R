source("scripts/function_get_lagbufferednpn_percentage.R")
npn_leaf <- get_buffle_npn_percentage(50,1,"Quercus",10)
npn_leaf_lag <- get_buffle_npn_percentage(50,1,"Quercus",10,TRUE)
npn_flower <- get_buffle_npn_percentage(50,7,"Quercus",10)
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


site_gg <- vector(mode = "list")

correlation_table <- to_plot %>%
  mutate(pollen = replace_na(pollen, 0)) %>% 
  group_by(station, year) %>%
  summarise(cor_leaf = cor(leaf, pollen, method = "spearman"),
            cor_leaf_lag = cor(leaf_lag, pollen, method = "spearman"),
            cor_flower = cor(flower, pollen, method = "spearman"))


for (i in seq_along(stationlist)) {
  
  
  to_plot_station <- to_plot %>% 
    filter(station == stationlist[i]) %>% 
    mutate(pollen = replace_na(pollen, 0)) 
  
  cor_data <- to_plot_station %>%
    group_by(year) %>%
    summarize(leaf_correlation = cor(leaf, pollen, method = "spearman",  use = "na.or.complete"),
              flower_correlation = cor(flower, pollen, method = "spearman",  use = "na.or.complete"),
              leaf_lag_correlation = cor(leaf_lag, pollen, method = "spearman",  use = "na.or.complete")) %>% 
    mutate(station = stationlist[i])
  
  site_gg[[i]] <-  to_plot_station %>% 
    ggplot() +
    geom_line(aes(x = doy, y = pollen, color = "pollen"), linewidth = 2, alpha = 0.75) +
    geom_line(aes(x = doy, y = leaf, color = "leaf"), linewidth = 1.5, alpha = 0.75) +
    geom_line(aes(x = doy, y = flower, color = "flower"), linewidth = 1, alpha = 0.75) +
    geom_line(aes(x = doy, y = leaf_lag, color = "leaf_lag"), linewidth = 0.5, alpha = 0.75) +
    facet_wrap(~ year)+
    geom_text(data = cor_data, 
              aes(x = Inf, y = Inf, 
                  label = paste0("leaf_corr: ", round(leaf_correlation, 2), "\n",
                                 "leaf_lag_corr: ", round(leaf_lag_correlation, 2), "\n",
                                 "flower_corr: ", round(flower_correlation, 2))), 
              hjust = 1, vjust = 1, size = 4) +
    ggtitle(stationlist[i])
}


pdf("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/lag_sperc_Smooth_Quercus_leafflowerpollen.pdf", width = 8, height = 8 * .618)
print(site_gg)
dev.off()


write_rds(site_gg,"/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/lag_RMSE_Smooth_Quercus_leafflowerpollen.rds")
write_csv(correlation_table,"/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/delete_npn_repeat_conflict/lag_RMSE_Smooth_Quercus_leafflowerpollen.csv")

  