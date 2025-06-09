# adapted from complete_herbarium.qmd in the main branch

## link phenology to raw data --------
raw <- read.csv(.path$herb_meta) %>%
  dplyr::select(day, month, year, startDayOfYear, decimalLongitude, decimalLatitude, filename_image, species, genus, family)

phenology <- read.csv(.path$herb_pheno) %>%
  mutate(filename_image = gsub(".txt", "", file_name))

joint_data <- left_join(phenology, raw, by = "filename_image")

## fillter flowering time --------

joint_data_flower <- joint_data %>% # 116805
  filter(flower_one > 0 & flower_many > 0) %>%
  filter(!is.na(startDayOfYear)) %>%
  filter(year >= 1895) %>%
  dplyr::select(decimalLongitude, decimalLatitude, startDayOfYear, year, species, genus, family, file_name, filename_image) %>%
  filter(family != "Pinaceae" & family != "Cupressaceae") %>% # delete family Pinaceae and Cupressaceae
  rename(lon = decimalLongitude, lat = decimalLatitude, doy = startDayOfYear) %>%
  distinct() # clear herbarium data for repeat file and repeat file with different phenology

# two repeated component:
# 1. completely the same ~300
# 2. same specimen (different name) with different phenology (as long as the flower is consistent, we will keep them) ~2000

write.csv(joint_data_flower, .path$herb_flower)
