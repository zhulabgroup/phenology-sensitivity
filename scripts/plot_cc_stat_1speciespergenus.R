if (!exists("data")) {
  source("scripts/function_npn_select_model_data.R")
  data <- get_modelled_data()
}

library(patchwork)
library(rnpn)
species_code <- rnpn::npn_species()

create_plot <- function(data,name) {
  data %>% group_by(species_id) %>%
    summarise(obs_count = n()) %>%
    filter(obs_count == max(obs_count)) %>%
    slice(1) %>%
    inner_join(data, by = "species_id") %>% 
    ggplot() +
    geom_point(aes(x = x, y = y), col = "blue", pch = 16) +
    geom_point(aes(x = x, y = y_fit), col = "red", pch = 16) +
    ggtitle(paste(name,
      species_code[species_code$species_id == data$species_id[1], "common_name"],
      "Coefficient:",
      sprintf("%.3f", data$coefficient[[1]]$cor),
      "P_value:",
      sprintf("%.3f", data$coefficient[[1]]$p.value),
      sep = " "
    )
      ) +
    scale_y_continuous(labels = function(y) format(as.Date("2000-01-01") + y - 1, "%b %d"))+
    scale_x_continuous(labels = function(y) format(as.Date("2000-01-01") + y - 1, "%b %d")) +
    xlab("Leafing Day") +
    ylab("Flowering Day")

}

site_gg <- map2(data, names(data), ~ create_plot(.x, .y))

combined_plot <- wrap_plots(site_gg, nrow = 4, ncol = 2)




