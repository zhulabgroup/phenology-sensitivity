library(raster)
# read herb and npn dat
npn_flower <- read.csv(.path$npn_flower) %>% 
  dplyr::select(lon, lat, year, doy, species, genus, taxa) %>%
  mutate(dataset = "npn")
herb_flower <- read.csv(.path$herb_flower) %>% 
  dplyr::select(lon, lat, year, doy, species, genus) %>%
  mutate(taxa = genus,
         dataset = "herb")
joint_data_flower <- rbind(npn_flower, herb_flower) 


# extract the climate normality ---------
complete_period_raster <- raster(.path$prism_norm)

joint_data_flower_normality <- joint_data_flower %>%
  dplyr::select(lat, lon) %>%
  distinct() %>%
  mutate(complete_period_temp = raster::extract(complete_period_raster, cbind(lon, lat)))

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
  yearly_raster <- raster(paste0(.path$prism_anom, fo_year, "_springmean.tif"))
  
  # Process the joint_data_flower for the current year
  yearly_data <- joint_data_flower %>%
    dplyr::select(year, lat, lon) %>%
    distinct() %>%
    filter(year == fo_year) %>%
    mutate(yearly_temp = raster::extract(yearly_raster, cbind(lon, lat)))
  
  # Append the yearly data to the cumulative data frame
  joint_data_flower_anormality <- rbind(joint_data_flower_anormality, yearly_data)
}

# Combine the normality and anormality data
temperature_data <- joint_data_flower_reframe %>%
  right_join(joint_data_flower_normality, by = c("lat", "lon")) %>%
  right_join(joint_data_flower_anormality, by = c("lat", "lon","year")) %>%
  rename(norm = complete_period_temp, yeart = yearly_temp) %>%
  mutate(anom = yeart - norm) %>%
  filter(!is.na(anom)) 

write.csv(temperature_data, .path$temperature_data)