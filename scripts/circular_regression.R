
library(circular)
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

ggplot(df_circ_fit)+
  geom_point(aes(x = x_off, y = y_off))+
  geom_line(aes(x = x_off, y = y_fit_off))+
  geom_hline(yintercept = 1, alpha = 0.1)+
  geom_vline(xintercept = 1, alpha = 0.1)+
  theme_classic()
