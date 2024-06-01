# Two strategies for multivariate visualization: grouping and faceting

# Grouping
# Grouping allow visualize the relationship or non-relationship from two numeric variables
# across the levels of one/two categorical variable, anothe numeric variable or a combination of both.
# 1 categorical
# 2 categorical
# 1 numeric
# 1 categorical + 1 numeric

library(tidyverse)
diamonds <- read_csv("datasets/diamonds - diamonds.csv")
head(diamonds)


# Basic scatterplot
carat_price <- diamonds %>% 
  ggplot(aes(x = carat, y = log(price))) +
  geom_point()
carat_price

# Grouping 1 categorical
carat_price + 
  geom_point(aes(color = color))

# There are several ways to do this, e.g. we can 
# include the grouping variable in the aesthetic mapping
carat_price <- diamonds %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point(aes(color = color))
carat_price

# Grouping 2 categorical
# Here we select a sample of 1000 observations
# The idea is to avoid overplotting 
diamonds_sample <- diamonds %>% 
  sample_n(1000)

carat_price <- diamonds_sample %>%
  ggplot(aes(x = carat, y = price)) +
  geom_point(aes(color = color, shape = clarity))
carat_price

# Grouping 1 numeric
# This is known as bubble plot
# Color might be omitted
carat_price <- diamonds_sample %>% ggplot(aes(x = carat, y = price)) +
  geom_point(aes(color = depth, size = depth, alpha = 0.6))
carat_price

# Grouping 1 categorical + 1 numeric
# Here color must be included as the categorical variable
carat_price <- diamonds_sample %>% ggplot(aes(x = carat, y = price)) +
  geom_point(aes(color = color, size = depth, alpha = 0.6))
carat_price


# Here a successful employment of grouping
iris <- read_csv("datasets/iris.csv")
head(iris)

iris_plot <- iris %>% ggplot(aes(x = sepal.length, y = sepal.width)) +
  geom_point(aes(color = variety, size = petal.length, alpha = 0.6))
iris_plot

# One way to solve overplotting 1 categorical
carat_price <- diamonds %>% 
  ggplot(aes(x = carat, y = price, color = color)) +
  geom_point() +
  geom_smooth(se=FALSE, 
              method = "lm", 
              formula = y~poly(x,2), 
              linewidth = 1.5) 
carat_price


# Faceting
# Faceting allow visualize a plot across the levels of one/two categorical variables
# In a few words, faceting is a split a plot into a matrix of panels according to the levels of one/two categorical variables

# 1 categorical
price_carat <- diamonds %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  facet_wrap(~color)

price_carat

# 2 categorical
price_carat <- diamonds %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  facet_grid(clarity~color)

price_carat

# Another plot by facet
price_carat <- diamonds %>% 
  ggplot(aes(x = price)) +
  geom_histogram() +
  facet_wrap(~color, ncol=1) 

price_carat
