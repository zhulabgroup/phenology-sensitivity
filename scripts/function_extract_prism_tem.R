# function of extracting prism spring temperature normal and anomaly

# first from month to yearly spring average temperature --------------------------------------------
library(snow)
library(prism)
library(raster)

# Initialize the cluster with the number of available cores
num_cores <- parallel::detectCores() - 1
cl <- makeCluster(num_cores)

# Export necessary variables to the cluster
clusterExport(cl, c("startyear", "endyear", "prism_archive_subset", "pd_stack", "writeRaster"))

# Load libraries and set the prism download directory on each worker node
clusterEvalQ(cl, {
  library(prism)
  library(raster)
  prism_set_dl_dir("data/prism")
})

# Function to process each year
process_year <- function(focal_year) {
  tmean_rast_yr_mo <- prism_archive_subset(temp_period = "monthly", type = "tmean", years = focal_year, mon = 3:5)
  tmean_rast2_yr_mo <- pd_stack(tmean_rast_yr_mo)
  r_mean <- raster::calc(tmean_rast2_yr_mo, mean)
  writeRaster(r_mean, paste0("data/prism/", focal_year, "_springmean.tif"))
}

# Apply the function in parallel
parLapply(cl, startyear:endyear, process_year)

# Stop the cluster
stopCluster(cl)

# second from yearly to decade spring average temperature: be careful the last group is 2015-2023 inclusing 9 years --------------------------------------------
# Re-initialize the cluster for decadal aggregation
cl <- makeCluster(num_cores)

# Export necessary variables to the cluster
clusterExport(cl, c("startyear", "endyear", "writeRaster"))

# Load libraries and set the prism download directory on each worker node
clusterEvalQ(cl, {
  library(raster)
})

# Function to aggregate decades
process_decade <- function(decade_start) {
  decade_years <- seq(decade_start, decade_start + 9)
  yearly_rasters <- lapply(decade_years, function(year) {
    raster_path <- paste0("data/prism/", year, "_springmean.tif")
    if (file.exists(raster_path)) {
      raster(raster_path)
    } else {
      NULL
    }
  })
  # Remove NULL values (years without data)
  yearly_rasters <- Filter(Negate(is.null), yearly_rasters)
  if (length(yearly_rasters) > 0) {
    decade_mean <- raster::calc(stack(yearly_rasters), mean)
    writeRaster(decade_mean, paste0("data/prism/", decade_start, "-", decade_start + 9, "_springmean.tif"))
  }
}

# Define decades
decades <- seq(startyear, endyear, by = 10)

# Apply the function in parallel
parLapply(cl, decades, process_decade)

# Stop the cluster
stopCluster(cl)

# third from decade to full-period spring average temperature: be careful the last group is 2015-2023 inclusing 9 years --------------------------------------------

aggregate_decades <- function(decades) {
  decade_rasters <- lapply(decades, function(decade_start) {
    raster_path <- paste0("data/prism/", decade_start, "-", decade_start + 9, "_springmean.tif")
  })
    period_mean <- raster::calc(stack(decade_rasters), mean)
    writeRaster(period_mean, "data/prism/complete_period_springmean.tif")
}

# Aggregate decade files into a complete period average raster
aggregate_decades(decades)

# fourth extract tem by lat lon and year --------------------------------------------
# Load complete period average raster



# Save the results to a CSV file
write.csv(temperature_data, "data/temperature_data.csv", row.names = FALSE)
