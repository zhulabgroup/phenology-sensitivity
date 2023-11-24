
plot_temporal_sensitive <- function(standarized_data, variable){
  
  #standarized_data <- individual_aggre
  #variable <- "leaf"
  # Fit linear model with no intercept
  mydata <- data.frame(
    x = standarized_data$spring_avg_temp,
    y = standarized_data[, variable]
  )
  
  names(mydata)[2] <- c("y")
  
  model <- lm(y ~ x - 1, data = mydata)
  
  # Extract slope
  slope <- coef(model)
  
  temporal <- ggplot(mydata, aes(x = x, y = y)) +
    geom_point() +
    geom_smooth(
      method = "lm", 
      formula = y ~ x - 1,  # Linear model with intercept forced to zero
      se = TRUE,           # Show uncertainty area (confidence interval)
      color = "blue"       # You can choose the color
    ) +
    geom_text(aes(x = max(x), y = max(y), 
                  label = paste("Slope:", round(slope, 2))),
              hjust = 1, vjust = 1, color = "black", size = 4) +
    labs(
      x = "spring_avg_temp",
      y = variable
    )
  
  return(temporal)
}

# Example usage:
# plot <- plot_spatial_sensitive(standarized_data, "YourVariableName")
# print(plot)
