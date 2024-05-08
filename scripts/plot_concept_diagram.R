library(tidyverse)

# Combined data frame
black <- data.frame(x = c(0, 1), y = c(1, 0), color = "black")
purple_center <- data.frame(x = seq(0.2, 0.8, by = 0.2), y = 1 - seq(0.2, 0.8, by = 0.2))

# Function to calculate line segments
calculate_segments <- function(center, slope, length) {
  start_x <- center$x - length / 2 * cos(atan(slope))
  start_y <- center$y - length / 2 * sin(atan(slope))
  end_x <- center$x + length / 2 * cos(atan(slope))
  end_y <- center$y + length / 2 * sin(atan(slope))
  
  tibble(start_x = start_x, start_y = start_y, end_x = end_x, end_y = end_y)
}

# Apply the function to all centers
segments_1 <- bind_rows(lapply(1:nrow(purple_center), function(i) 
  calculate_segments(purple_center[i, ], -1.5, 0.18)))
segments_2 <- bind_rows(lapply(1:nrow(purple_center), function(i) 
  calculate_segments(purple_center[i, ], -1, 0.18)))
segments_3 <- bind_rows(lapply(1:nrow(purple_center), function(i) 
  calculate_segments(purple_center[i, ], -0.5, 0.18)))
# Plot
p1 <- ggplot(mapping = aes(x = x, y = y)) + 
  geom_line(data = black, aes(group = 1), color = "black") +
  geom_segment(data = segments_1, aes(x = start_x, y = start_y, xend = end_x, yend = end_y), color = "purple", size = 2) +
  xlab("Average spring temperature") +
  ylab("Leafing day") +
  ggtitle("Temporal stronger than spatial") +
  theme_classic() + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"))
p2 <- ggplot(mapping = aes(x = x, y = y)) + 
  geom_line(data = black, aes(group = 1), color = "black") +
  geom_segment(data = segments_2, aes(x = start_x, y = start_y, xend = end_x, yend = end_y), color = "purple", size = 2) +
  xlab("Average spring temperature") +
  ylab("Leafing day") +
  ggtitle("Temporal equals to spatial") +
  theme_classic() + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"))
p3 <-  ggplot(mapping = aes(x = x, y = y)) + 
  geom_line(data = black, aes(group = 1), color = "black") +
  geom_segment(data = segments_3, aes(x = start_x, y = start_y, xend = end_x, yend = end_y), color = "purple", size = 2) +
  xlab("Average spring temperature") +
  ylab("Leafing day") +
  ggtitle("Temporal weaker than spatial") +
  theme_classic() + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"))
library(patchwork)
p1+p2+p3