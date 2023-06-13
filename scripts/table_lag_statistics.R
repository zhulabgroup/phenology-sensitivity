if (!exists("data")) {
source("scripts/function_npn_select_model_data.R")
data <- get_modelled_data()
}

remove_outliers <- function(x, threshold, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=threshold, na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  x[x < (qnt[1] - H) | x > (qnt[2] + H)] <- NA
  return(x)
}

delete_grass <- data[setdiff(names(data), "Poaceae")]
combined_table_lag <- NULL

for (i in seq_along(delete_grass) ) {
  
   lag_table <- delete_grass[[i]] %>%
     group_by(species_id) %>%
     summarise(
       genus = names(data)[i],
       lagday_average = mean(remove_outliers(lag,c(.1, .9)), na.rm = TRUE),
       lagday_std = sd(remove_outliers(lag,c(.1, .9)), na.rm = TRUE),
       lagday_std_normal = sd(remove_outliers(lag,c(.1, .9)), na.rm = TRUE)/(max(remove_outliers(lag,c(.1, .9)), na.rm = TRUE)-min(remove_outliers(lag,c(.1, .9)), na.rm = TRUE)),
       laggdd_average = mean(remove_outliers(laggdd,c(.1, .9)), na.rm = TRUE),
       laggdd_std = sd(remove_outliers(laggdd,c(.1, .9)), na.rm = TRUE),
       laggdd_std_normal = sd(remove_outliers(laggdd,c(.1, .9)), na.rm = TRUE)/(max(remove_outliers(laggdd,c(.1, .9)), na.rm = TRUE)-min(remove_outliers(laggdd,c(.1, .9)), na.rm = TRUE))
     ) 
   

   combined_table_lag <- bind_rows(combined_table_lag, lag_table)
}

# tem <- as.tibble(laggdd_table$std_dev/ combined_table_lag$std_dev)



