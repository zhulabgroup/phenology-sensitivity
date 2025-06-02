library(raster)
# read herb and npn dat
npn_flower <- read.csv("../data/npn_flower.csv") %>% 
  dplyr::select(lon, lat, year, doy, species, genus, family, taxa) %>%
  mutate(dataeset = npn)
herb_flower <- read.csv("../data/herb_flower.csv") %>% 
  dplyr::select(lon, lat, year, doy, species, genus, family, taxa) %>%
  mutate(dataeset = herb)
joint_data_flower <- rbind(npn_flower, herb_flower) 

# extract the climate normality ---------
complete_period_raster <- raster("../data/prism/complete_period_springmean.tif")

joint_data_flower_normality <- joint_data_flower %>%
  dplyr::select(lat, lon) %>%
  distinct() %>%
  mutate(complete_period_temp = extract(complete_period_raster, cbind(lon, lat)))

# extract the climate anormality

## reshape doy to number of days since Nov 1st ------

joint_data_flower_reframe <- joint_data_flower %>%
  mutate(doy = doy + 61) %>%
  mutate(year = ifelse(doy > 365, year+1, year),
         doy = ifelse(doy > 365, doy - 365, doy)) 
# Initialize an empty data frame to store the results
joint_data_flower_anormality <- data.frame()

# Loop through the specified years
for (fo_year in 1895:2023) {
  # Load the yearly raster file
  yearly_raster <- raster(paste0("../data/prism/", fo_year, "_springmean.tif"))
  
  # Process the joint_data_flower for the current year
  yearly_data <- joint_data_flower %>%
    dplyr::select(year, lat, lon) %>%
    distinct() %>%
    filter(year == fo_year) %>%
    mutate(yearly_temp = extract(yearly_raster, cbind(lon, lat)))
  
  # Append the yearly data to the cumulative data frame
  joint_data_flower_anormality <- rbind(joint_data_flower_anormality, yearly_data)
}

# Combine the normality and anormality data
temperature_data <- joint_data_flower_reframe %>%
  right_join(joint_data_flower_anormality, by = c("lat", "lon")) %>%
  rename(norm = complete_period_temp, yeart = yearly_temp) %>%
  mutate(anom = yeart - norm) %>%
  right_join(joint_data_flower, by = c("lat", "lon", "year")) %>%
  filter(!is.na(anom)) 

write.csv(temperature_data, "../data/temperature_data.csv")