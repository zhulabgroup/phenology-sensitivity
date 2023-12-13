# function to get yearly winter and spring temperature for given observations
library(daymetr)

get_winter_spring_temperatures <- function(locations_data) {
  results <- data.frame()

  for (i in 1:nrow(locations_data)) {
    lon <- locations_data[i, ] %>% pull(longitude) 
    lat <- locations_data[i, ] %>% pull(latitude) 
    myyear <- locations_data[i, ] %>% pull(year)
    
    # Use tryCatch to handle errors and skip the loop iteration if an error occurs
    tryCatch({
      daymet_data <- download_daymet(lon = lon, lat = lat, start = myyear - 1, end = myyear)
      
      temperature <- daymet_data$data %>% 
        select("tmin..deg.c.", "tmax..deg.c.", "yday", "year") %>%
        mutate(avertem = rowMeans(select(., c("tmin..deg.c.", "tmax..deg.c.")), na.rm = TRUE)) 
      
      winter <- temperature %>% 
        filter((year == myyear - 1 & yday > 354) | (year == myyear & yday < 80)) %>% 
        summarise(winter_avg_temp = mean(avertem))
      
      spring <- temperature %>% 
        filter(year == myyear & yday > 79 & yday < 173) %>% 
        summarise(spring_avg_temp = mean(avertem))
      
      location_result <- data.frame(
        longitude = lon,
        latitude = lat,
        year = myyear,
        winter_avg_temp = winter$winter_avg_temp,
        spring_avg_temp = spring$spring_avg_temp
      )
      
      results <- rbind(results, location_result)
      
    }, error = function(e) {
      cat("Error occurred in iteration", i, "Skipping...\n")
    })
  }
  
 
  
  return(results)
}


