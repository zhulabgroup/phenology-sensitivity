create_combined_plot <- function(data, x_vars, y_vars) {
  max_abs_x <- max(abs(data[, x_vars]))
  max_abs_y <- max(abs(data[, y_vars]))
  
  plots <- lapply(1:length(x_vars), function(i) {
    x_var <- x_vars[i]
    y_var <- y_vars[i]
    
    slope <- coef(MASS::rlm(data[[y_var]] ~ data[[x_var]] - 1))
    
    p <- ggplot(data, aes(x = data[[x_var]], y = data[[y_var]], color = common_name)) +
      geom_point(alpha = 0.1) +
      geom_abline(intercept = 0, slope = slope, color = "red") +
      geom_text(aes(x = max(data[[x_var]]), y = max(data[[y_var]]), label = paste("Slope:", round(slope, 2))),
                hjust = 1, vjust = 1, color = "red", size = 4) +
      labs(
        x = x_var,
        y = y_var
      ) +
      theme_minimal() + 
      theme(legend.position = "none") + 
      ylim(-max_abs_y, max_abs_y) + 
      xlim(-max_abs_x, max_abs_x)
    
    return(p)
  })
  
  combined_plot <- wrap_plots(plots, ncol = 2)
  
  return(combined_plot)
}