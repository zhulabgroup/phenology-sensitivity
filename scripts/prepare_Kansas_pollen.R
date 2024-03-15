library(ggplot2)
library(maps)

station_id <- read.csv("/Volumes/seas-zhukai/phenology/nab/clean/2023-04-25/renew_station_info.csv")

lat = station_id$lat
lon = station_id$lon
# Create a data frame
data <- data.frame(lat, lon)

# US map data
us <- map_data("state")

# Plot the map
ggplot() +
  geom_polygon(data = us, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_point(data = data, aes(x = lon, y = lat), color = "red", size = 3) +
  labs(title = "Latitude and Longitude Points on US Map",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

# Detroit too few data
# Children's Mercy Hospital Kansas City 086cc3e6-3cb7-474a-acb8-83ecaddd2018

raw_data <- read_rds("/Volumes/seas-zhukai/phenology/nab/clean/2023-04-25/nab_renew_raw.rds")

selected_data <- raw_data %>% 
  filter(`Station ID` == "086cc3e6-3cb7-474a-acb8-83ecaddd2018") %>% 
  mutate(total = rowSums(.[5:77], na.rm = TRUE)) %>% 
  select(Date, total)

# Extract year and day of year from the Date column
selected_data <- selected_data %>%
  mutate(Year = lubridate::year(Date),
         DayOfYear = as.numeric(format(Date, "%j")))

complete_days <- expand.grid(Year = unique(selected_data$Year), DayOfYear = 1:365)

# Left join with selected_data to fill in missing days with NA values
complete_data <- left_join(complete_days, selected_data, by = c("Year", "DayOfYear"))

# Arrange the data by Year and DayOfYear
complete_data <- complete_data %>%
  arrange(Year, DayOfYear)

# Create a wide-format data frame
wide_data <- complete_data %>%
  select(Year, DayOfYear, total) %>%
  pivot_wider(names_from = DayOfYear, values_from = total)

write.csv(wide_data,"/Volumes/seas-zhukai/phenology/phenology_pollen/New_data/Kansas_total.csv")
# clean_data <- read_rds("/nfs/turbo/seas-zhukai/phenology/nab/clean/2023-04-25/nab_renew.rds")

selected_data_by_species <- raw_data %>%
  filter(`Station ID` == "086cc3e6-3cb7-474a-acb8-83ecaddd2018") %>%
  select(Date, 5:77)

# Replace NA values with 0
selected_data_by_species[is.na(selected_data_by_species)] <- 0

# Complete the Date column with every day and fill with NA
complete_dates <- data.frame(Date = seq(min(selected_data_by_species$Date), max(selected_data_by_species$Date), by = "day"))
selected_data_by_species <- complete_dates %>%
  left_join(selected_data_by_species, by = "Date")

write.csv(selected_data_by_species,"/Volumes/seas-zhukai/phenology/phenology_pollen/New_data/Kansas_by_species.csv")

