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

# choose locations
locations <- dplyr::select(joint_data_flower, lat, lon) %>%
  distinct() 

locations$site_id <- seq_len(length(locations))

prism_dir <- "prism_data"

# list ALL monthly tmean bil files
bil_files <- list.files(
  prism_dir,
  pattern = "PRISM_tmean_.*_4kmM3_\\d{6}_bil$",
  full.names = TRUE
)

# extract year and month from folder name
file_index <- tibble(
  path = bil_files,
  yyyymm = gsub(".*_(\\d{6})_bil$", "\\1", bil_files),
  year = as.integer(substr(yyyymm, 1, 4)),
  month = as.integer(substr(yyyymm, 5, 6))
) |>
  filter(
    year >= 1895,
    year <= 2023,
    month %in% c(2, 3)
  ) |>
  arrange(year, month)

extract_one_file <- function(i, index_df, sites_sp) {
  
  bil_path <- index_df$path[i]
  
  bil_file <- list.files(
    bil_path,
    pattern = "\\.bil$",
    full.names = TRUE
  )
  
  if (length(bil_file) == 0) return(NULL)
  
  r <- raster(bil_file)
  
  vals <- raster::extract(r, sites_sp)
  
  data.frame(
    site_id = sites_sp$site_id,
    year    = index_df$year[i],
    month   = index_df$month[i],
    tmean   = vals
  )
}


n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)

clusterEvalQ(cl, library(raster))
clusterExport(cl, c("extract_one_file", "file_index", "locations"))

results <- parLapply(
  cl,
  seq_len(nrow(file_index)),
  extract_one_file,
  index_df = file_index,
  sites_sp = locations
)

stopCluster(cl)

prism_feb_mar <- bind_rows(results) 

prism_feb_mar$tmean <- prism_feb_mar$tmean

coords <- as.data.frame(coordinates(locations))
coords$site_id <- locations$site_id
names(coords) <- c("lon", "lat", "site_id")

final_out <- prism_feb_mar |>
  left_join(coords, by = "site_id") |>
  group_by(lat, lon, year) |>
  summarise(
    feb_mar_tmean = mean(tmean, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(year, lat)

head(final_out)

# now calculate by location the average over the full period and make name it norm and also calcualte the deviation off norm for each year
norms <- final_out |>
  group_by(lat, lon) |>
  summarise(
    norm = mean(feb_mar_tmean, na.rm = TRUE),
    .groups = "drop"
  )

## reshape doy to number of days since Nov 1st ------

joint_data_flower_reframe <- joint_data_flower %>%
  mutate(doy = doy + 61) %>%
  mutate(year = ifelse(doy > 365, year+1, year),
         doy = ifelse(doy > 365, doy - 365, doy)) 

# Combine the normality and anormality data
temperature_data <- joint_data_flower_reframe %>%
  right_join(norms, by = c("lat", "lon")) %>%
  right_join(final_out, by = c("lat", "lon","year")) %>%
  rename(yeart = feb_mar_tmean) %>%
  mutate(anom = yeart - norm) %>%
  filter(!is.na(anom)) 

write.csv(temperature_data, .path$temperature_data)