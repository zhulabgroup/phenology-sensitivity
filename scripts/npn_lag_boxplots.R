source("scripts/npn_select+model_data.R")

data <- get_modelled_data()
combined_table_lag <- NULL
combined_table_laggdd <- NULL

for (i in seq_along(data) ) {
  
   lag_table <- data[[i]] %>%
     group_by(species_id) %>%
     summarise(
       average = mean(lag),
       std_dev = sd(lag)
     )
   
   laggdd_table <- data[[i]] %>%
     group_by(species_id) %>%
     summarise(
       average = mean(lagadd),
       std_dev = sd(lagadd)
     )

   combined_table_lag <- bind_rows(combined_table_lag, lag_table)
   combined_table_laggdd <- bind_rows(combined_table_laggdd, laggdd_table)
}


tem <- as.tibble(laggdd_table$std_dev/ combined_table_lag$std_dev)