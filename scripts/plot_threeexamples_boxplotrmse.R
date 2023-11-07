rmse_table <- read_csv("data/delete_npn_repeat_conflict/lag_RMSE_Smooth_Quercus_leafflowerpollen.csv") %>% 
  dplyr::select(leaf_lag_rmse, flower_rmse) %>% 
  rename(leaf = leaf_lag_rmse, flower = flower_rmse) %>% 
  pivot_longer(cols = c("leaf", "flower"), names_to = "pheno", values_to = "rmse")


count_data <- rmse_table %>%
  group_by(pheno) %>%
  summarise(n = sum(!is.na(rmse)))


boxplot_lf <- rmse_table %>%
  ggplot(aes(x = pheno, y = rmse)) +
  geom_boxplot() +
  geom_text(data = count_data, aes(label = n, y = 0.1), vjust = -0.5, size = 5) +
  theme_minimal() +
  labs(x = " ", y = "RMSE") +
  scale_y_continuous(limits = c(0, 0.105)) +
  scale_x_discrete(labels = c("Leaf\nPollen", "Flower\nPollen"))

#### get station figures
source("scripts/function_get_lagbufferednpn_percentage.R")
npn_leaf_lag <- get_buffle_npn_percentage(50,1,"Quercus",10,TRUE)[[1]]
npn_flower <- get_buffle_npn_percentage(50,7,"Quercus",10)[[1]]
source("scripts/function_get_smoothed_nab_for_taxa.R")
nab <- get_smoothed_nab("Quercus")

# stardarize the data (should be put into getting data?)
standardize_data <- function(data, new_name) {
  renamed_data <- data %>% 
    filter((station == "67e7f6ab-b262-45a0-b592-e71cbf433b2f" & year == 2017) | 
            (station == "6261714f-e5ae-41ef-96a7-0b1c108256a2" & year == 2016) |
            (station == "d22bab5b-768e-4bcc-a70f-8ebe9ef7cb4d" & year == 2017)) %>% 
    dplyr::select(station, year, doy, count_w) %>% 
   rename({{new_name}} := count_w)
  
  unified_data <- renamed_data %>% 
    group_by(station, year) %>% 
    summarise(total = sum({{new_name}})) %>%
    inner_join(renamed_data, by = c("station","year")) %>% 
    mutate({{new_name}} := {{new_name}} / total) %>% 
    dplyr::select(-total) %>% 
    ungroup()
  
  
  return(unified_data)
}

npn_leaf_lag_s <- standardize_data(npn_leaf_lag, leaf_lag) 
npn_flower_s <- standardize_data(npn_flower, flower)
nab_s <- standardize_data(nab %>% 
                            mutate(doy = lubridate::yday(date))%>% 
                            rename(station = stationid), pollen)

#find station name
station_info <- read_csv("/nfs/turbo/seas-zhukai/phenology/nab/clean/2023-04-25/renew_station_info.csv") %>% 
  mutate(station_name = paste(city, state, sep = ", ")) %>% 
  dplyr::select(station_name,id) %>% 
  rename(station = id)

to_plot <- npn_leaf_lag_s %>% 
  full_join(npn_flower_s, by = c("station", "year", "doy")) %>% 
  left_join(nab_s,by = c("station","year","doy")) %>% 
  group_by(station,year) %>%
  filter(!all(is.na(pollen))) %>% 
    mutate(pollen = replace_na(pollen, 0))  %>% 
  left_join(station_info, by = "station") %>% 
  rename(leaf=leaf_lag)


rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2/max(actual) - min(actual), na.rm = TRUE))
}



cor_data <- to_plot %>%
  group_by(station_name) %>%
  summarize(leaf_rmse = rmse(leaf, pollen),
            flower_rmse = rmse(flower, pollen)) 


desired_order <- c("San Jose, CA",
                   "Mount Laurel, NJ", 
                   "Springfield, NJ")



three_examples <- to_plot %>% 
    ggplot() +
    geom_line(aes(x = doy, y = pollen, color = "Pollen"), linewidth = 1, alpha = 0.75) +
    geom_line(aes(x = doy, y = flower, color = "Flower"), linewidth = 1, alpha = 0.75) +
    geom_line(aes(x = doy, y = leaf, color = "Leaf"), linewidth = 0.5, alpha = 0.75) +
  facet_wrap(~ factor(station_name, levels = desired_order), ncol = 1) +    
  geom_text(data = cor_data, 
              aes(x = Inf, y = Inf, 
                  label = paste0("leaf_pollen_rmse: ", round(leaf_rmse, 2), "\n",
                                 "flower_pollen_rmse: ", round(flower_rmse, 2))), 
              hjust = 1, vjust = 1, size = 4) +
  labs(x = "Day of year", y = "Normalized index")+
  scale_color_manual(name = "Phenology", values = c("Flower" = "purple", "Leaf" = "green", "Pollen" = "black"))+
  theme_bw()


three_examples_boxplot <- boxplot_lf + three_examples +
  patchwork::plot_annotation(tag_levels = "A") +
  patchwork::plot_layout(design = 
                           "ABBB
                            ABBB
                            ABBB"
  )

