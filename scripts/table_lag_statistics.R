source("scripts/function_npn_select_model_data.R")

data <- get_modelled_data()

combined_table_lag <- NULL
combined_table_laggdd <- NULL

for (i in seq_along(data) ) {
  
   lag_table <- data[[i]] %>%
     group_by(species_id) %>%
     summarise(
       average = mean(remove_outliers(lag,c(.1, .9)), na.rm = TRUE),
       std_dev = sd(remove_outliers(lag,c(.1, .9)), na.rm = TRUE)
     )
     
   
   # laggdd_table <- data[[i]] %>%
   #   group_by(species_id) %>%
   #   summarise(
   #     average = mean(remove_outliers(lag),c(.25, .75)),
   #     std_dev = sd(remove_outliers(lag),c(.25, .75))
   #   )
  

   combined_table_lag <- bind_rows(combined_table_lag, lag_table)
   #combined_table_laggdd <- bind_rows(combined_table_laggdd, laggdd_table)
}


remove_outliers <- function(x, threshold, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=threshold, na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  x[x < (qnt[1] - H) | x > (qnt[2] + H)] <- NA
  return(x)
}

tem <- as.tibble(laggdd_table$std_dev/ combined_table_lag$std_dev)
