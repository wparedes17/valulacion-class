# group visualization
# 1. boxplot / violin plot
# 2. spyder plot or radar plot
# 3. chernoff face plot

library(tidyverse)
library(ggChernoff)

# 1. boxplot / violin plot
diamonds <- read.csv("datasets/diamonds - diamonds.csv")
x11()
# violin plot
diamonds %>% 
  ggplot(aes(x = cut, y = price, fill = cut)) +
  geom_violin() +
  theme_bw()

# Log transformation
diamonds %>% 
  ggplot(aes(x = cut, y = log(price), fill = cut)) +
  geom_violin() +
  theme_bw()


# spyder plot or radar plot
diamonds_cut <- diamonds %>% 
  mutate(
    price_sd = (log(price) - mean(log(price)))/sd(log(price)), 
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

diamonds_spyder <- diamonds_cut %>%ggplot(
    aes(x = Variables, y = Scores, col = cut, group = cut)) + 
  geom_polygon(fill = NA) + coord_polar() + facet_wrap(~cut)

diamonds_spyder

# 3. Chernoff face plot
diamonds_face <-diamonds_cut %>% ggplot(
    aes(x = 1, y = Variables, smile = Scores, group = Scores)) + 
  geom_chernoff(fill = 'goldenrod1') + facet_wrap(~cut)

diamonds_face

# 4. Another Chernoff face plot
p <- iris %>% 
  ggplot(aes(Sepal.Width, Sepal.Length, fill = Species, brow = Sepal.Length, smile)) +
  geom_chernoff() + 
  scale_brow_continuous(midpoint = min) + 
  scale_brow_continuous(range = c(-.5, 2))

p
