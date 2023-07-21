# diagram for method
source("scripts/function_get_lagbufferednpn_percentage.R")
npn_leaf <- get_buffle_npn_percentage(50,1,"Quercus",10)
npn_leaf_lag <- get_buffle_npn_percentage(50,1,"Quercus",10,TRUE)


lag <- read_csv("/nfs/turbo/seas-zhukai/phenology/phenology_leaf_flower_lag/oak_species_lag.csv")


sample_station <- points_within_buffer_filtered[["5effe609-c645-4620-bc99-a3b34934897c"]] %>% 
  filter(year == 2022)

create_species_plot <- function(target_id) {
  plot <- sample_station %>%
    filter(species_id == target_id) %>%
    ggplot() +
    geom_tile(aes(x = doy, y = as.factor(individual_id), fill = factor(phenophase_status))) +
    scale_fill_manual(values = c("grey", "brown")) +
    ylab('individual') +
    theme(
      axis.text.y = element_blank(),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white"),
      plot.margin = margin(1, 1, 1, 1, unit = "cm"),
      plot.title = element_text(size = 14),
      legend.position = "none"
    ) +
    ggtitle(lag$common_name[lag$species_id == target_id])
  
  return(plot)
}

# Species IDs to loop through
species_ids <- c(1159,102, 100)

# Create a list of plots for each species
species_plots <- map(species_ids, create_species_plot)

# Arrange the plots vertically using patchwork
arranged_plots <- wrap_plots(species_plots, nrow = length(species_plots))
arranged_plots

arranged_plots <- plot_grid(plotlist = species_plots, ncol = 1)

# create a figure for log all

with_lag <- sample_station %>% 
  left_join(lag, "species_id") %>% 
  mutate(lagged = doy+avelag) %>% 
  filter(species_id %in% species_ids) 

with_lag %>%
  ggplot() +
  geom_tile(aes(x = doy, y = reorder(individual_id, species_id), fill = factor(phenophase_status))) +
  scale_fill_manual(values = c("grey", "brown")) +
  ylab('individual') +
  theme(
    axis.text.y = element_blank(),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    plot.title = element_text(size = 14),
    legend.position = "none"
  ) 

# figure for percentage point
test <- with_lag %>% 
  group_by(doy) %>% 
  summarise(percentage = mean(phenophase_status)) %>% 
  ggplot() +
  geom_point(aes(x = doy, y = percentage))
