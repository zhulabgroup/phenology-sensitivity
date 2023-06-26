# check whether our metrics of sos is reasonable
library(tidyverse)
library(lubridate)
library(zoo)

nab_acer <- read_rds("/nfs/turbo/seas-zhukai/phenology/nab/clean/2023-04-25/nab_renew.rds") %>% filter(taxa=="Acer")

nab_acer$date <- as.Date(nab_acer$date)  # Convert date column to Date object

nab_completed <- nab_acer %>%
  group_by(stationid) %>%
  complete(date = seq.Date(min(date), max(date), by = "day"), fill = list(count = NaN)) %>%
  arrange(date) %>%
  mutate(count_smoothed = rollmean(count, k = 7, fill = NA, align = "center", na.rm = TRUE)) %>% 
  ungroup()


unique_stations <- unique(nab_completed$stationid)
site_gg <- vector(mode = "list")

for (i in seq_along(unique_stations)) {
  station <- unique_stations[i]
  # Subset data for the current station_id
  subset_data <- nab_completed %>%
    filter(stationid == station) %>% 
    filter(count_smoothed > 50)
  
  # Check if the subset_data is not empty
  if (nrow(subset_data) > 0) {
    # Create the plot for non-empty data
    site_gg[[i]] <- subset_data %>%
      ggplot() +
      geom_point(aes(x = yday(date), y = count), size = 1, alpha = 0.3) +
      geom_line(aes(x = yday(date), y = count_smoothed), color = "red") +
      facet_wrap(~ year(date)) +
      labs(x = "doy", y = "count") +
      ggtitle(station) +
      theme_bw()
  }
}
site_gg <- site_gg[!sapply(site_gg, is.null)]

pdf("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/pollen_season_50.pdf", width = 8, height = 8 * .618)
print(site_gg)
dev.off()
