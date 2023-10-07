# This script is used to create subplots in R
library(tidyverse)
library(cowplot)


# Some analysis procedure
diamonds <- read.csv("datasets/diamonds - diamonds.csv")
head(diamonds)

# Create individual plots
# Here we create two plots: p1 and p2
p1 <- diamonds %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  theme_bw()

p2 <- diamonds %>% 
  ggplot(aes(x = cut)) +
  geom_bar() +
  theme_bw()

# Combine the plots using cowplot
# ggdraw() is used to create a blank canvas
# draw_plot() is used to draw the plots on the canvas
# sizes are specified in relative units
my_plots <- ggdraw() + 
    draw_plot(p1, x = 0, y = 0, width = 0.5, height = 1) +
    draw_plot(p2, x = 0.5, y = 0, width = 0.5, height = 1) 

my_plots   
