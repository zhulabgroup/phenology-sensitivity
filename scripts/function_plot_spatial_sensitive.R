
plot_spatial_sensitive <- function(standarized_data, variable){
  
  #standarized_data <- individual_aggre
  #variable <- "leaf"
  # Fit linear model with no intercept
  mydata <- data.frame(
    x = standarized_data$spring_avg_temp_Mean,
    x_max = standarized_data$spring_avg_temp_Max,
    x_min = standarized_data$spring_avg_temp_Min,
    y = standarized_data[, paste0(variable, "_Mean")],
    y_max = standarized_data[, paste0(variable, "_Max")],
    y_min = standarized_data[, paste0(variable, "_Min")]
  )
  
  # Rename the columns
  names(mydata)[4:6] <- c("y", "y_max", "y_min")
  
  
  model <- lm(y ~ x - 1, data = mydata)
  
  # Extract slope
  slope <- coef(model)
  
  spatial <- ggplot(mydata, aes(x = x, y = y)) +
    geom_point() +
    geom_errorbarh(
      aes(
        xmin = x_min + x,
        xmax = x_max + x,
        y = y
      ),
      height = 0.1,  # Adjust as needed
      alpha = 0.2
    ) +
    geom_errorbar(
      aes(
        ymin = y_min + y,
        ymax = y_max + y,
        x = x
      ),
      width = 0.1,  # Adjust as needed
      alpha = 0.2
    ) +
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
  
  return(spatial)
}

# Example usage:
# plot <- plot_spatial_sensitive(standarized_data, "YourVariableName")
# print(plot)
