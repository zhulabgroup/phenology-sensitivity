get_smoothed_nab <- function(specifictaxa){
# get nab smoothed
  library(zoo)
  library(lubridate)
  
  nab <- read_rds("/nfs/turbo/seas-zhukai/phenology/nab/clean/2023-04-25/nab_renew.rds") %>%
    filter(taxa == specifictaxa) %>%
    mutate(date = as.Date(date)) %>%
    mutate(year = year(date))
  
  filtered_group <- nab  %>%
    group_by(stationid, year) %>%
    summarise(num_obs = n(), 
              leng_obs = as.numeric(max(date) - min(date) + 1), 
              num_50 = sum(count>50)) %>% 
    filter(num_50 > 3 & num_obs > 36 & leng_obs > 100) 
  
  nab_smooth <- filtered_group %>%
    dplyr::select(stationid,year) %>% 
    left_join(nab, by = c("stationid", "year")) %>% 
    group_by(stationid, year) %>% 
    complete(date = seq.Date(min(date), max(date), by = "day"), fill = list(count = NA)) %>%
    mutate(count_l = na.approx(count, maxgap = 14),
           count_l = replace(count_l, is.na(count_l), 0),
           count_w = ptw::whit1(count_l, lambda = 10)) 
    # arrange(date) %>%
    # mutate(count_smoothed = rollmean(count, k = 7, fill = NA, align = "center", na.rm = TRUE)) %>%
    #   ungroup() %>%
    #   rename(station = stationid)
  # %>%
  #   filter(count_smoothed > threshold) %>%
  #   slice(1) 
  # 
  return(nab_smooth) 
}

