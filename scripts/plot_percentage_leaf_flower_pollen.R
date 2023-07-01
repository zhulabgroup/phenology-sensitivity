library(tidyverse)
source("scripts/function_get_bufferednpn_percentage.R")
npn_leaf <- get_buffle_npn_percentage(50,1,"Quercus",10)
npn_flower <- get_buffle_npn_percentage(50,7,"Quercus",10)
source("scripts/function_get_smoothed_nab_for_taxa.R")
nab <- get_smoothed_nab("Quercus")

# stardarize the data (should be put into getting data?)
npn_leaf_s <- npn_leaf %>% 
  select(station,year, doy,count_w) %>% 
  rename(leaf = count_w)

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

# check statistics
leaf_joined <- inner_join(npn_leaf_s, nab_s, by = c("station","year","doy"))
flower_joined <- inner_join(npn_flower_s, nab_s, by = c("station","year","doy"))


  correlation_leaf <- leaf_joined %>%
    group_by(station, year) %>%
    filter(!(all(leaf == first(leaf)) || all(pollen == first(pollen)))) %>% # avoid warning: the standard deviation is zero
    summarise(cor_leaf = ifelse(n() >= 5, cor(leaf, pollen, method = "spearman"), NA))
  
  correlation_flower <- flower_joined %>%
    group_by(station, year) %>%
    filter(!(all(flower == first(flower)) || all(pollen == first(pollen)))) %>% 
    summarise(cor_flower = ifelse(n() >= 5, cor(flower, pollen, method = "spearman"), NA))
  
  ggplot() +
    geom_violin(data = correlation_flower, aes(x = "flower", y = cor_flower)) +
    geom_boxplot(data = correlation_flower, aes(x = "flower", y = cor_flower), width = 0.2, fill = "white", color = "black") +
    geom_violin(data = correlation_leaf, aes(x = "leaf", y = cor_leaf)) +
    geom_boxplot(data = correlation_leaf, aes(x = "leaf", y = cor_leaf), width = 0.2, fill = "white", color = "black") +
    scale_x_discrete(labels = c("flower", "leaf"))
  
  
  same_year <- inner_join(correlation_flower,correlation_leaf,by = c("station","year")) 
  
  ggplot(same_year) +
    geom_violin(aes(x = "flower", y = cor_flower)) +
    geom_boxplot(aes(x = "flower", y = cor_flower), width = 0.2, fill = "white", color = "black") +
    geom_violin(aes(x = "leaf", y = cor_leaf)) +
    geom_boxplot(aes(x = "leaf", y = cor_leaf), width = 0.2, fill = "white", color = "black") +
    scale_x_discrete(labels = c("flower", "leaf"))
  
  same_year %>%
    ggplot(aes(x = cor_leaf, y = cor_flower)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") 
  
  sum(same_year,cor_leaf>cor_flower)
  sum_same_year <- sum(same_year$cor_leaf > same_year$cor_flower)
  
# visial check
to_plot <- full_join(npn_leaf_s,npn_flower_s,by = c("station","year","doy"))  %>% 
  left_join(nab_s,by = c("station","year","doy")) %>% 
  group_by(station,year) %>%
  filter(!all(is.na(pollen)))

stationlist <- unique(to_plot$station)

site_gg <- vector(mode = "list")
correlation_table <- tibble(year = integer(), leaf_correlation = numeric(), flower_correlation = numeric(), station = character())

for (i in seq_along(stationlist)) {
  to_plot_station <- to_plot %>% 
    filter(station == stationlist[i])
   
  cor_data <- to_plot_station %>%
    group_by(year) %>%
    summarize(leaf_correlation = cor(leaf, pollen, method = "spearman",  use = "na.or.complete"),
              flower_correlation = cor(flower, pollen, method = "spearman",  use = "na.or.complete")) %>% 
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
                  label = paste0("leaf_correlation: ", round(leaf_correlation, 2), "\n", 
                                 "flower_correlation: ", round(flower_correlation, 2))), 
              hjust = 1, vjust = 1, size = 4) +
    ggtitle(stationlist[i])
}


pdf("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/Smooth_Quercus_leafflowerpollen.pdf", width = 8, height = 8 * .618)
print(site_gg)
dev.off()

# statistics corresponding to figures
leaf_paris <- sum(!is.na(correlation_table$leaf_correlation)) # 175
flower_paris <- sum(!is.na(correlation_table$flower_correlation)) # 127
num_pairs <- sum(!is.na(correlation_table$leaf_correlation) & !is.na(correlation_table$flower_correlation)) #123
sum_diff_positive <- sum(correlation_table$leaf_correlation - correlation_table$flower_correlation >= 0, na.rm = TRUE) # 46/123 = 0.37


ggplot(correlation_table) +
  geom_violin(aes(x = "flower", y = flower_correlation)) +
  geom_boxplot(aes(x = "flower", y = flower_correlation), width = 0.2, fill = "white", color = "black") +
  geom_violin(aes(x = "leaf", y = leaf_correlation)) +
  geom_boxplot(aes(x = "leaf", y = leaf_correlation), width = 0.2, fill = "white", color = "black") +
  scale_x_discrete(labels = c("flower", "leaf"))

  