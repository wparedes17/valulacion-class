library(tidyverse)

advertising <- read_csv("datasets/advertising.csv")
head(advertising)

# Basic scatterplot
scatterplot <- advertising %>% 
  ggplot(aes(x = TV, y = Sales)) +
  geom_point()

scatterplot

# Basic scatterplot
scatterplot <- advertising %>% 
  ggplot(aes(x = Radio, y = Sales)) +
  geom_point()

scatterplot


# Basic scatterplot
scatterplot <- advertising %>% 
  ggplot(aes(x = Newspaper, y = Sales)) +
  geom_point()

scatterplot

# Basic scatterplot
scatterplot <- advertising %>% 
  ggplot(aes(x = Newspaper, y = Radio)) +
  geom_point()

scatterplot
