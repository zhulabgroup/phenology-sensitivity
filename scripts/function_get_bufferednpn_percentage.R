get_buffle_npn_percentage <- function(buffer, phenoclass, taxa, lam){
  
library(sf)
library(zoo)
  
# get npn data
npn_phenophases <- rnpn::npn_phenophases() 

id <- npn_phenophases[npn_phenophases$pheno_class_id==phenoclass,"phenophase_id"]

npn_wind <- read_rds(paste0("/nfs/turbo/seas-zhukai/phenology/NPN/wind_poll_taxa/", taxa, ".rds")) %>%
  filter(phenophase_id %in% id$phenophase_id) %>%
  select(latitude, longitude, individual_id, observation_date, phenophase_status, species_id) %>%
  mutate(observation_date = as.Date(observation_date),
         year = year(observation_date),
         doy = yday(observation_date)) %>% 
  filter(phenophase_status > -1)

individual_list <- distinct(npn_wind, longitude, latitude)

# get nab station
station_info <- read.csv("/nfs/turbo/seas-zhukai/phenology/nab/clean/2023-04-25/renew_station_info.csv") %>% 
  filter(country=="US")

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

# Create a buffer of 50km around each station
station_buffers <- st_buffer(stations_sf, dist = buffer*1000)  # 200000 meters = 200 kilometers

# Create an empty list to store points within each station buffer
points_within_buffer <- vector("list", length = nrow(stations_sf))

# Iterate over each station
for (i in 1:nrow(stations_sf)) {
  station_buffer <- station_buffers[i, ]
  
  # Find points within the station buffer
  temp <- st_intersection(points_sf, station_buffer)
  points_within_buffer[[i]] <- npn_wind %>%
    semi_join(temp, by = c("latitude", "longitude"))
}

names(points_within_buffer) <- station_info$id

points_within_buffer_filtered <- keep(points_within_buffer, ~ nrow(.x) > 0)

npn <- map(points_within_buffer_filtered, function(df) {
df %>%
    group_by(observation_date) %>%
    summarize(percentage = mean(phenophase_status)) %>% 
    rename(date = observation_date) %>% 
    mutate(year = year(date),
           doy = yday(date)) %>% 
    group_by(year) %>% 
    filter(!all(percentage == 0)) %>% 
    filter(n()>3)
}) %>%
  enframe(name = "station", value = "data")  %>%
  unnest(data) %>% 
  select(-date) %>% 
  group_by(station, year) %>% 
  complete(doy = seq(min(doy), max(doy)), fill = list(percentage = NA)) %>% 
  mutate(count_l = na.approx(percentage, maxgap = 14),
         count_l = replace(count_l, is.na(count_l), 0),
         count_w = ptw::whit1(count_l, lambda = lam)) %>% 
  complete(doy = 1:365, fill = list(count_l = 0, count_w = 0)) %>% 
  ungroup()

return(npn) 
}

