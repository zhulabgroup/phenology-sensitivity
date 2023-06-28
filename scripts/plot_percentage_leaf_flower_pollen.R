library(tidyverse)
source("scripts/function_get_bufferednpn_percentage.R")
npn_leaf <- get_buffle_npn_percentage(50,7,"Quercus")
source("scripts/function_get_smoothed_nab_for_taxa.R")
nab <- get_smoothed_nab("Quercus")


library(scales)

site_gg <- vector(mode = "list")
correlation_table <- tibble(year = integer(), correlation = numeric(), station = character())

for (i in seq_along(npn)) {
  stationid <- names(npn[i])
  
  station_npn <- npn[[i]]
  
  station_nab <- nab %>% filter(station == stationid)
  
  joint_data <- left_join(station_nab,station_npn,by = "date") %>% 
    group_by(year) %>%
    filter(!all(is.na(percentage))) %>%
    ungroup()
  
  cor_data <- joint_data %>%
    group_by(year) %>%
    summarize(correlation = ifelse(all(is.na(count) | is.na(percentage)), NA, cor(count, percentage, method = "spearman", use = "complete.obs"))) %>% 
    mutate(station = stationid)
  
  correlation_table <- bind_rows(correlation_table, cor_data)
  
  # Plot the data
  site_gg[[i]] <- ggplot(joint_data) +
    geom_point(aes(x = yday(date), y = percentage), alpha = 0.2) +
    geom_point(aes(x = yday(date), y = rescale(count)), alpha = 0.2, color = "red") +
    geom_line(aes(x = yday(date), y = rescale(count_smoothed)), color = "red") +
    facet_wrap(~ year) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "Day of Year", y = "Standardized Value") +
    geom_text(data = cor_data, aes(x = Inf, y = Inf, label = paste0("Correlation: ", round(correlation, 2))), hjust = 1, vjust = 1, size = 4)+
    ggtitle(stationid)
  
}
site_gg <- keep(site_gg, function(plot) nrow(plot$data) > 0)

pdf("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/Quercus_reproduce_flower_pollen.pdf", width = 8, height = 8 * .618)
print(site_gg)
dev.off()




write_csv(correlation_table,"/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/Quercus_reproduce_flower_pollen.csv")

flower <- read.csv("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/Quercus_reproduce_flower_pollen.csv") %>% 
  filter(!is.na(correlation))
leaf <- read.csv("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/Quercus_reproduce_leaf_pollen.csv") %>% 
  filter(!is.na(correlation))

station_groups <- full_join(flower, leaf, by = c("station", "year"))

station_groups_inner <- inner_join(flower, leaf, by = c("station", "year"))
ggplot(station_groups_inner) +
  geom_violin(aes(x = "correlation.x", y = correlation.x)) +
  geom_violin(aes(x = "correlation.y", y = correlation.y)) +
  xlab("") +
  ylab("Correlation") +
  labs(x = "") +
  ggtitle("Correlation Distribution") +
  scale_x_discrete(labels = c("correlation.x", "correlation.y")) +
  theme_bw()


ggplot(station_groups_inner) +
  geom_violin(aes(correlation.x))+
  geom_violin(aes(correlation.y))

combined_data <- rbind(leaf, flower)
combined_data$group <- c(rep("leaf", nrow(leaf)), rep("flower", nrow(flower)))

ggplot(combined_data, aes(x = group, y = correlation)) +
  geom_violin()

ggplot(combined_data, aes(x = group, y = correlation)) +
  geom_point()

station_groups %>%
  ggplot(aes(x = correlation.x, y = correlation.y)) +
  geom_point() +
  xlab("Correlation (flower)") +
  ylab("Correlation (leaf)") +
  ggtitle("Correlation Comparison")+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") 
  