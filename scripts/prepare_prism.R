# Prepare PRISM data for anom and norm extraction

library(snow)
library(prism)
library(raster)

startyear <- 1895
endyear <- 2024

# ---- Yearly winter (Dec prev year + Jan–Feb current year) ------------
# ---- First year: Jan–Feb only

library(parallel)

# Initialize the cluster
num_cores <- parallel::detectCores() - 1
cl <- makeCluster(num_cores)

# Export necessary objects
clusterExport(
  cl,
  c("startyear", "endyear", "prism_archive_subset", "pd_stack", "writeRaster")
)

# Load libraries and set PRISM directory on workers
clusterEvalQ(cl, {
  library(prism)
  library(raster)
  prism_set_dl_dir("prism_data")
})

# Function to process each year
process_year <- function(focal_year) {
  
  if (focal_year == startyear) {
    # First year: January–February only
    tmean_all <- prism_archive_subset(
      temp_period = "monthly",
      type = "tmean",
      resolution = "4km",
      years = focal_year,
      mon = 1:2
    )
    
  } else {
    # Other years: December (prev year) + Jan–Feb (current year)
    tmean_dec_prev <- prism_archive_subset(
      temp_period = "monthly",
      type = "tmean",
      resolution = "4km",
      years = focal_year - 1,
      mon = 12
    )
    
    tmean_jf_curr <- prism_archive_subset(
      temp_period = "monthly",
      type = "tmean",
      resolution = "4km",
      years = focal_year,
      mon = 1:2
    )
    
    tmean_all <- c(tmean_dec_prev, tmean_jf_curr)
  }
  
  # Stack and calculate mean
  tmean_stack <- pd_stack(tmean_all)
  r_mean <- raster::calc(tmean_stack, mean, na.rm = TRUE)
  
  # Write output
  writeRaster(
    r_mean,
    paste0("data/prism/", focal_year, "_wintermean.tif"),
    overwrite = TRUE
  )
}

# Apply in parallel
parLapply(cl, startyear:endyear, process_year)

# Stop cluster
stopCluster(cl)

# ---- Then from yearly to decade winter mean ------------

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
    raster_path <- paste0("data/prism/", year, "_wintermean.tif")
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
    writeRaster(decade_mean, paste0("data/prism/", decade_start, "-", decade_start + 9, "_wintermean.tif"))
  }
}

# Define decades
decades <- seq(startyear, endyear, by = 10)

# Apply the function in parallel
parLapply(cl, decades, process_decade)

# Stop the cluster
stopCluster(cl)

# third from decade to full-period spring average temperature: be careful the last group is 2015-2023 including 9 years --------------------------------------------

aggregate_decades <- function(decades) {
  decade_rasters <- lapply(decades, function(decade_start) {
    raster_path <- paste0("data/prism/", decade_start, "-", decade_start + 9, "_wintermean.tif")
  })
  period_mean <- raster::calc(stack(decade_rasters), mean)
  writeRaster(period_mean, "data/prism/complete_period_wintermean.tif")
}

# Aggregate decade files into a complete period average raster
aggregate_decades(decades)

