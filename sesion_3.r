library(tidyverse)

diamonds <- read_csv("datasets/diamonds - diamonds.csv")
glimpse(diamonds)

# Convert all character variables into factor
diamonds <- diamonds %>% mutate_if(is.character, factor)

# Summary
summary(diamonds)

# Summary of one variable
summary(diamonds$carat) # way 1
summary(diamonds %>% pull(carat)) # way 2

# Compute one element of the summary
min(diamonds %>% pull(carat)) # minimum
mean(diamonds %>% pull(carat)) # mean
mean(diamonds %>% pull(carat), trim = 0.1) # trimmed mean
