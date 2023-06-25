library(tidyverse)
 
# prepare acer nab data

nab_acer <- read_rds("/nfs/turbo/seas-zhukai/phenology/nab/clean/2023-04-25/nab_renew.rds") %>% filter(taxa=="Acer")


# prepare acer npn data

station_info <- read.csv("/nfs/turbo/seas-zhukai/phenology/nab/clean/2023-04-25/renew_station_info.csv") %>% filter(country=="US")

npn_acer <- read_rds("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/climate/Acer.rds")

npn_acer_qc <- npn_acer %>% filter(observed_status_conflict_flag == "-9999") %>% # Removing Status Conflicts
  group_by(individual_id, first_yes_year, pheno_class_id) %>%
  arrange(first_yes_doy) %>%
  slice(1)%>%
  mutate(numdays_since_prior_no = na_if(numdays_since_prior_no, "-9999")) %>% # set the -9999 values to NA
  filter(numdays_since_prior_no < 20) %>% # Filtering Data by Prior No
  dplyr::select(individual_id, first_yes_year, first_yes_doy, species_id, dataset_id, pheno_class_id, gdd, longitude, latitude) %>%
  filter(pheno_class_id == 1) %>% 
  ungroup()

individual_list <- distinct(npn_acer_qc, longitude, latitude)

library(sf)

# Convert data frames to sf objects
stations_sf <- st_as_sf(station_info, 
                      coords = c("lon", "lat"), 
                      crs = 4326, 
                      agr = "constant", 
                      remove = FALSE)
points_sf <- st_as_sf(individual_list, 
                      coords = c("longitude", "latitude"), 
                      crs = 4326, 
                      agr = "constant", 
                      remove = FALSE)

# Create a buffer of 200km around each station
station_buffers <- st_buffer(stations_sf, dist = 200000)  # 200000 meters = 200 kilometers

# Create an empty list to store points within each station buffer
points_within_buffer <- vector("list", length = nrow(stations_sf))

# Iterate over each station
for (i in 1:nrow(stations_sf)) {
  station_buffer <- station_buffers[i, ]
  
  # Find points within the station buffer
  temp <- st_intersection(points_sf, station_buffer)
  points_within_buffer[[i]] <- npn_acer_qc %>%
    semi_join(temp, by = c("latitude", "longitude"))
}

names(points_within_buffer) <- station_info$id

library(lubridate)

site_gg <- vector(mode = "list")

for (i in seq_along(points_within_buffer) ) {

  id <- names(points_within_buffer[i])
  
  sample_nab <- nab_acer %>% filter(stationid==id)
  
  site_gg[[i]] <- ggplot() + 
    geom_tile(data = sample_nab, aes(x = yday(date), y = year(date), fill = ifelse(count == 0, 0, log(count)))) + # loged
    scale_fill_gradient(low = "white", high = "brown") +
    labs(x = "Day of year", y = "Year") +
    geom_point(data = test, aes(x = first_yes_doy, y = first_yes_year))
}

pdf("/nfs/turbo/seas-zhukai/phenology/NPN/leaf_flower/leaf_pollen.pdf", width = 8, height = 8 * .618)
print(site_gg)
dev.off()