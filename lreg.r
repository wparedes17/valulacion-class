library(tidyverse)

# Load dataset
advertising <- read_csv("datasets/advertising.csv")
glimpse(advertising)

# Simple linear regresion TV vs Sales
# This computes the linear regression of Sales on TV
lm1 <- lm(Sales ~ TV, data = advertising)
summary(lm1)

# We are going to create a new table with the TV values
# In this case, in a sequence from the minimum to the maximum of the observed values
# The goal is have the same number of rows as the original dataset to plot the regression line
new_advertising <- tibble(
  TV = seq(min(advertising$TV), max(advertising$TV), length.out = nrow(advertising))
)
# Now, we are going to predict the Sales values using the linear model
# The model for this example is Sales = 6.9748 + 0.0555 * TV
# In the context of the data,
# this mean that for each 1,000 euros spent in TV advertising,
# the sales increase in 0.0555 million of euros
new_advertising <- new_advertising %>%
  mutate(NewSales = predict(lm1, newdata = new_advertising))

advertising %>%
  ggplot(aes(x = TV, y = Sales)) +
  geom_point() +
  geom_line(data = new_advertising, aes(y = NewSales), col = "red")

# Now, we are goin to work with a dataset with categorical variables
# We are going to use the dataset "diamonds"

# Load dataset
diamonds <- read_csv("datasets/diamonds - diamonds.csv")
glimpse(diamonds)

# We are going to compute a model to explain the price of the diamonds using the cut
# Even when chr variable is handled by lm, it is better to convert it to a factor
diamonds$cut <- as.factor(diamonds$cut)
# This lines is only informative, it is not necessary
levels(diamonds$cut)
# Relevel enables to change the reference level of the factor
# Reflevel is used as the base level for the regression when intercept is not set as 0
diamonds$cut <- relevel(diamonds$cut, ref = "Ideal")
lm2 <- lm(price ~ cut, data = diamonds)
summary(lm2)

# When we using only one factor in a regression model,
# the coefficients are the means of each category.
diamonds %>%
  group_by(cut) %>%
    summarise(mean_price = mean(price))

# Y = a + mx

# Y = a + {m2 * I(cat=2) + m3 * I(cat=3)}

# Y = m1 * I(cat=1) + m2 * I(cat=2) + m3 * I(cat=3)