library(patchwork)

compare_plots <- function(data, column1, column2,yname) {
  compare_figure_1 <- ggplot(data) +
    geom_point(aes(x = !!rlang::sym(column1), y = !!rlang::sym(column2))) +
    geom_abline(intercept = 0, slope = 1, color = "red")
  
  compare_figure_2 <- data %>%
    mutate(difference = !!rlang::sym(column1) - !!rlang::sym(column2)) %>%
    ggplot(aes(x = reorder(species_id, difference), y = difference)) +
    geom_col() +
    coord_flip() +
    ylab(yname) +
    xlab("species")
  
  compare_figure <- compare_figure_1 + compare_figure_2
  
  return(compare_figure)
}

gdd_doy <- compare_plots(combined_table_lag, "lagday_std_normal", "laggdd_std_normal", "day - gdd")

leaf_climate <- compare_plots(combined_table_lag, "flogdd_std_normal", "laggdd_std_normal", "climate - leaf")

