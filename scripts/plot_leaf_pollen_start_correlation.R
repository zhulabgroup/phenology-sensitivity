if (!exists("points_within_buffer")) {
  source("scripts/function_generate_nab_npn_buffer.R")
  points_within_buffer <- get_buffle_data(200)
}
library(lubridate)

# prepare npn data
npn <- map(points_within_buffer, function(df) {
    df %>%
      group_by(first_yes_year) %>%
      filter(n() >= 3) %>%
      summarize(average_first_yes_doy = mean(first_yes_doy))
  }) %>% 
  keep(~ nrow(.x) > 0) %>%
  enframe(name = "station", value = "data")  %>%
  unnest(data) %>% 
  rename(year = first_yes_year)


# prepare acer nab data

nab_acer <- read_rds("/nfs/turbo/seas-zhukai/phenology/nab/clean/2023-04-25/nab_renew.rds") %>% filter(taxa=="Acer")

nab <- map2(npn$station, npn$year, function(station, year) {
  nab <- nab_acer %>%
    filter(stationid == station, year(date) == year)
  
  first_date <- if (nrow(nab) > 200) {
    nab %>%
      mutate(count_smoothed = zoo::rollmean(count, k = 7, fill = NA, align = "center")) %>%
      filter(count_smoothed > 50) %>%
      pull(date) %>%
      min(na.rm = TRUE)
  } else {
    NA
  }
  
  if (is.infinite(first_date)) {
    first_date <- NA
  }
  
  data.frame(station = station, year = year, first_date = first_date)
}) %>% 
  enframe(value = "data") %>% 
  unnest(data) %>% 
  dplyr::select(-name)

joint_data <- inner_join(npn, nab, by = c("station", "year"))

ggplot(joint_data)+
  geom_point(aes(x = average_first_yes_doy, y = yday(first_date)))

cor(joint_data$average_first_yes_doy, yday(joint_data$first_date),use = "complete.obs", method = "spearman")
