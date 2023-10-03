# group visualization
# 1. boxplot / violin plot
# 2. spyder plot or radar plot

library(tidyverse)
library(ggChernoff)

# 1. boxplot / violin plot
diamonds <- read.csv("datasets/diamonds - diamonds.csv")

# violin plot
diamonds %>% 
  ggplot(aes(x = cut, y = price, fill = cut)) +
  geom_violin() +
  theme_bw()


# spyder plot or radar plot
diamonds_cut <- diamonds %>% 
  mutate(
    price_sd = (price - mean(price))/sd(price), 
    carat_sd = (carat - mean(carat))/sd(carat), 
    depth_sd = (depth - mean(depth))/sd(depth), 
    table_sd = (table - mean(table))/sd(table)) %>%
    group_by(cut) %>%
    summarise(
      price = mean(price_sd), 
      carat = mean(carat_sd), 
      depth = mean(depth_sd), 
      table = mean(table_sd))

diamonds_cut <- diamonds_cut %>% 
  gather(key = "Variables", value = "Scores", -cut) 
 
diamonds_cut %>%ggplot(
    aes(x = Variables, y = Scores, col = cut, group = cut)) + 
  geom_polygon(fill = NA) + coord_polar() + facet_wrap(~cut)


p <- ggplot(iris) +
aes(Sepal.Width, Sepal.Length, fill = Species, brow = Sepal.Length, smile) +
geom_chernoff()
x11()
p
p + scale_brow_continuous(midpoint = min)
p + scale_brow_continuous(range = c(-.5, 2))
