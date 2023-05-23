#explore circular_circular regression

library(tidyverse)
library(circular)


# library(ggplotly) #can't install
# Generate a data set of dependent circular variables.
x <- seq(1, 365, by = 5)
y <- c(x[-1:-10], x[1:10])

x <- c(1, 20, 50, 60, 300,345, 350, 365)
y <- c(5, 30, 50, 56, 320,350, 1,10 )

# method 1: write in Cartesian coordinate-----
angle_X <- circular(x * 360 / 365, units = "degrees", template = "geographics")
angle_Y <- circular(y * 360 / 365, units = "degrees", template = "geographics")

x_cartesian <- cbind(cos(angle_X*pi/180), sin(angle_X*pi/180))
y_cartesian <- cbind(cos(angle_Y*pi/180), sin(angle_Y*pi/180))

regression_model_y <- lm(y_cartesian ~ x_cartesian)

summary(regression_model_y)

# Getting the fitted values
fitted_values_y <- fitted(regression_model_y)

# Compute the angles from the fitted values
fitted_angles_y <- atan2(fitted_values_y[,2], fitted_values_y[,1]) * (180 / pi)

# Convert to a circular data type
fitted_circular_y <- circular(fitted_angles_y, units = "degrees", template = "geographics")

# Plotting x and y
plot(x, y, col = "blue", pch = 16, main = "Circular Regression")
# Adding x against fitted_circular_y
points(x, fitted_circular_y, col = "red", pch = 16)



ggplot() +
  geom_point(aes(x = x, y = y)) +
  geom_line(aes(x = x, y = fitted_circular_y)) +
  geom_hline(yintercept = 1, alpha = 0.1) +
  geom_vline(xintercept = 1, alpha = 0.1) +
  theme_classic()



plot(x,y)
plot(x,fitted_circular_y)
# method 2: cut off
# Generate a data set of dependent circular variables.
df<-data.frame(x = c(1, 20, 50, 60, 300,345, 350, 365),
               y = c(5, 30, 50, 56, 320,350, 1,10 ))

x_den<-density(df$x, from = 1, to = 365)
x_thres <- data.frame(x = x_den$x, y = x_den$y) %>%
  arrange(y) %>%
  head(1) %>%
  pull(x)

y_den<-density(df$y, from = 1, to = 365)
y_thres <- data.frame(x = y_den$x, y = y_den$y) %>%
  arrange(y) %>%
  head(1) %>%
  pull(x)

df_circ<- df %>%
  mutate(x_off = if_else (x > x_thres, x-365,x),
         y_off = if_else (y > y_thres, y-365, y)) %>%
  mutate(x_circ =(x_off/365*360) *pi/180 ,
         y_circ =(y_off/365*360) *pi/180 )

# Fit a circular-circular regression model.
circ.lm <- lm.circular(df_circ$y_circ, df_circ$x_circ, order=1)
circ.lm$p.values

df_circ_fit<-df_circ %>%
  mutate(y_fit_circ = circ.lm$fitted) %>%
  mutate(y_fit = y_fit_circ * 180/pi/360*365)  %>%
  mutate(y_fit_off = if_else(y_fit > y_thres, y_fit-365, y_fit) )

ggplot(df_circ_fit) +
  geom_point(aes(x = x_off, y = y_off)) +
  geom_line(aes(x = x_off, y = y_fit_off)) +
  geom_hline(yintercept = 1, alpha = 0.1) +
  geom_vline(xintercept = 1, alpha = 0.1) +
  theme_classic()

# Plotting x and y
plot(df_circ_fit$x_off, df_circ_fit$y_off, col = "blue", pch = 16, main = "Circular Regression")
# Adding x against fitted_circular_y
points(df_circ_fit$x_off, df_circ_fit$y_fit_off , col = "red", pch = 16)
