# input: format doy 1-365 
# output: x_thres, y_thres, tibble(x,y,predicted_y) there may be redundant because one x has mutiple y but same predicted y


mycircular <- function(x,y){
  
  library(circular)
  
  df<-tibble(x,y)
  
  x_den<-density(df$x, from = 1, to = 365)
  x_thres <- tibble(x = x_den$x, y = x_den$y) %>%
    arrange(y) %>%
    head(1) %>%
    pull(x)
  
  y_den<-density(df$y, from = 1, to = 365)
  y_thres <- tibble(x = y_den$x, y = y_den$y) %>%
    arrange(y) %>%
    head(1) %>%
    pull(x)
  
  df_circ<- df %>%
    mutate(x_off = if_else (x > x_thres, x-365, as.double(x)),
           y_off = if_else (y > y_thres, y-365, as.double(y))) %>%
    mutate(x_circ =(x_off/365*360) *pi/180 ,
           y_circ =(y_off/365*360) *pi/180 )
  
  # Fit a circular-circular regression model.
  circ.lm <- lm.circular(df_circ$y_circ, df_circ$x_circ, order=1)
  
  coefficient <- cor.circular(df_circ$y_circ, df_circ$x_circ, test = TRUE)
  
  df_circ_fit<-df_circ %>%
    mutate(y_fit_circ = circ.lm$fitted) %>%
    mutate(y_fit = y_fit_circ * 180/pi/360*365)  %>%
    mutate(y_fit_off = if_else(y_fit > y_thres, y_fit-365, as.double(y_fit)) ) 
  
  my_list <- list(
    x_thres = x_thres,
    y_thres = y_thres,
    x = df_circ_fit$x_off,
    y = df_circ_fit$y_off,
    y_fit = as.numeric(df_circ_fit$y_fit_off),
    coefficient = coefficient
  )
  
  return(my_list)
}