get_smoothed_nab <- function(specifictaxa){
# get nab smoothed
  library(tidyverse)
  library(zoo)
  library(lubridate)
  
  nab_acer <- read_rds("/nfs/turbo/seas-zhukai/phenology/nab/clean/2023-04-25/nab_renew.rds") %>%
    filter(taxa == specifictaxa) %>%
    mutate(date = as.Date(date)) %>%
    mutate(year = year(date)) %>%
    group_by(stationid, year) %>%
    complete(date = seq.Date(min(date), max(date), by = "day"), fill = list(count = NA)) %>%
    arrange(date) %>%
    mutate(count_smoothed = rollmean(count, k = 7, fill = NA, align = "center", na.rm = TRUE)) %>%
      ungroup() %>%
      rename(station = stationid)
  # %>%
  #   filter(count_smoothed > threshold) %>%
  #   slice(1) 
  # 
  return(nab_acer) 
}