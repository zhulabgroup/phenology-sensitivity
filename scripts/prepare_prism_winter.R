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