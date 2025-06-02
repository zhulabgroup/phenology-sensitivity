# use R to download the images and rename to "jpg format" (format requirement for leafmachine2)

devtools::install_github("tncvasconcelos/mvh")
library(mvh)

# Search for specimen metadata
metadata <- search_specimen_metadata(
  taxon_name = "Acer rubrum",
  coordinates = c(42.28, -83.74),
  limit = 8
)

# Download specimen images
download_specimen_images(
  metadata,
  dir_name = "Red_maple_in_AnnArbor_example/specimens",
  result_file_name = "Red_maple_in_AnnArbor_example/result_download"
)

# after download_specimen_images(...)
specimen_dir <- "Red_maple_in_AnnArbor_example/specimens"

# find all .jpeg files
jpeg_files <- list.files(specimen_dir, pattern = "\\.jpeg$", full.names = TRUE)

# construct new names and rename
new_names <- sub("\\.jpeg$", ".jpg", jpeg_files)
file.rename(from = jpeg_files, to = new_names)

write_csv2(metadata, "meta_data.csv")