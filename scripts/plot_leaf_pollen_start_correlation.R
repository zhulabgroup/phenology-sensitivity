library(lubridate)
library(zoo)

# get npn by station
if (!exists("points_within_buffer")) {
  source("scripts/function_generate_nab_npn_buffer.R")
  points_within_buffer <- get_buffle_data(50,7)
}

npn <- map(points_within_buffer, function(df) {
  df %>%
    group_by(first_yes_year) %>%
    filter(n() >= 3) %>%
    summarize(median_first_yes_doy = median(first_yes_doy))
}) %>% 
  keep(~ nrow(.x) > 0) %>%
  enframe(name = "station", value = "data")  %>%
  unnest(data) %>% 
  rename(year = first_yes_year)


# get nab smoothed
nab_acer <- read_rds("/nfs/turbo/seas-zhukai/phenology/nab/clean/2023-04-25/nab_renew.rds") %>% filter(taxa=="Acer")

nab_acer$date <- as.Date(nab_acer$date)  # Convert date column to Date object

nab <- nab_acer %>%
  mutate(year = year(date)) %>% 
  group_by(stationid, year) %>%
  complete(date = seq.Date(min(date), max(date), by = "day"), fill = list(count = NA)) %>%
  arrange(date) %>%
  mutate(count_smoothed = rollmean(count, k = 7, fill = NA, align = "center", na.rm = TRUE)) %>% 
  filter(count_smoothed > 50) %>% 
  slice(1) %>% 
  ungroup() %>% 
  rename(station = stationid) 


joint_data <- inner_join(npn, nab, by = c("station", "year"))
cor_data <- joint_data %>%
  group_by(station) %>%
  summarize(cor_coef = round(cor(median_first_yes_doy, yday(date)), 2))

ggplot(joint_data) +
  geom_point(aes(x = median_first_yes_doy, y = yday(date))) +
  facet_wrap(~ station) +
  geom_text(data = cor_data, aes(x = Inf, y = Inf, label = paste("Correlation:", cor_coef)),
            hjust = 1, vjust = 1, size = 4)

