if (!exists("data")) {
source("scripts/function_npn_select_model_data.R")
data <- get_modelled_data()
}

remove_outliers <- function(x, threshold = c(.1, .9), na.rm = TRUE, ...) {
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
     mutate(lag_out = remove_outliers(lag),
            laggdd_out = remove_outliers(laggdd),
            flogdd_out = remove_outliers(gdd_y)) %>%
     summarise(
       genus = names(data)[i],
       lagday_average = mean(lag_out, na.rm = TRUE),
       lagday_std = sd(lag_out, na.rm = TRUE),
       lagday_std_normal = sd(lag_out, na.rm = TRUE)/diff(range(lag_out, na.rm = TRUE)),
       laggdd_average = mean(laggdd_out, na.rm = TRUE),
       laggdd_std = sd(laggdd_out, na.rm = TRUE),
       laggdd_std_normal = sd(laggdd_out, na.rm = TRUE)/diff(range(laggdd_out, na.rm = TRUE)),
       flogdd_average = mean(flogdd_out, na.rm = TRUE),
       flogdd_std = sd(flogdd_out, na.rm = TRUE),
       flogdd_std_normal = sd(flogdd_out, na.rm = TRUE)/diff(range(flogdd_out, na.rm = TRUE))
     ) 
   

   combined_table_lag <- bind_rows(combined_table_lag, lag_table)
}

# tem <- as.tibble(laggdd_table$std_dev/ combined_table_lag$std_dev)



