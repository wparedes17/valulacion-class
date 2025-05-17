library(tidyverse)

advertising <- read_csv("./datasets/advertising.csv")
glimpse(advertising)

# Simple linear regresion TV vs Sales + Radio + Newspaper (multiple linear regression - full model)
# Fit a linear model
fit <- lm(Sales ~ TV + Radio + Newspaper, data = advertising)
# Check the summary of the model
summary(fit)

# These are the questions we are going to answer with the regression model:
# 1. Is at least one of the predictors X1, X2, . . . , Xp useful in predicting
# the response?
# Linear regression function: lm() computes an hypothesis test for the null
# hypothesis that all the coefficients are equal to 0. If the p-value is
# less than 0.05, we reject the null hypothesis and conclude that at least one
# of the predictors is useful in predicting the response.
# In this case, the p-value is less than 0.05 (less than 2.2e-16), so we reject the null 
# hypothesis and conclude that at least one of the predictors is useful in
# predicting the response.

# 2. Do all the predictors help to explain Y , or is only a subset of the
# predictors useful?
# Linear regression function: lm() computes an hypothesis test for each predictor
# in the model. If the p-value is less than 0.05, we reject the null hypothesis
# and conclude that the predictor is useful in predicting the response.
# In this case, the p-value for TV is less than 0.05 (less than 2.2e-16), so we
# reject the null hypothesis and conclude that TV is useful in predicting the
# response. The p-value for Radio is also less than 0.05 (less than 2.2e-16),
# so we reject the null hypothesis and conclude that Radio is useful in
# predicting the response. The p-value for Newspaper is greater than 0.05
# (0.954), so we fail to reject the null hypothesis and conclude that
# Newspaper is not useful in predicting the response. This means that we can
# remove Newspaper from the model and fit a new model with only TV and Radio
# as predictors.

# 3. How well does the model fit the data?
# Linear regression function: lm() computes the R-squared value, which is a
# measure of how well the model fits the data. The R-squared value is between 0
# and 1, where 0 means that the model does not fit the data at all and 1 means
# that the model fits the data perfectly. In this case, the R-squared value is
# 0.9026, which means that the model explains 90.26% of the variance in the
# response variable. This is a good fit, but there is still some unexplained
# variance in the data.
# R-squares = 1 - (RSS/TSS)

# Thumb rule: R-squared > 0.6 is a good fit, R-squared > 0.8 is a very good fit, 
# and R-squared > 0.9 is an excellent fit. However, the value of R-squared is context dependent.


# We define a new dataset with the values of the predictors
new_data <- tibble(
    TV = c(150, 200),
    Radio = c(20, 30),
    Newspaper = c(10, 15)
)

# We use the predict() function to predict the response variable for the new
# dataset. The interval argument specifies the type of interval to compute.
# We always user interval = "prediction" to compute the prediction interval instead of 
# the confidence interval because we want to predict the response variable for a new
# observation, not the mean response for a given set of predictor values.
predictions <- predict(fit, newdata = new_data, interval = "prediction", level = 0.99)
head(predictions)

# This is a way to visualize the prediction intervals for the new data
# We create a grid of values for the predictors
full_grid_data <- tibble(
    TV = seq(min(advertising$TV), max(advertising$TV), length.out = 200),
    Radio = seq(min(advertising$Radio), max(advertising$Radio), length.out = 200),
    Newspaper = seq(min(advertising$Newspaper), max(advertising$Newspaper), length.out = 200)
)

# We add the values of the predictors to the new dataset
predictions <- predict(fit, newdata = full_grid_data, interval = "prediction", level = 0.95)
advertising <- advertising %>%
    mutate(
        new_TV = full_grid_data$TV,
        fit = predictions[, "fit"],
        lwr = predictions[, "lwr"],
        upr = predictions[, "upr"]
    )

# We plot the data and the prediction intervals
# We use the ggplot2 package to create the plot
advertising %>%
    ggplot(aes(x = TV, y = Sales)) +
    geom_point() +
    geom_line(aes(x = new_TV, y = fit), color = "blue") +
    geom_line(aes(x = new_TV, y = lwr), color = "red", linetype = "dashed") +
    geom_line(aes(x = new_TV, y = upr), color = "red", linetype = "dashed") +
    labs(
        title = "Sales vs TV",
        x = "TV",
        y = "Sales"
    ) +
    theme_minimal()

# 4. Given a set of predictor values, what response value should we predict,
# and how accurate is our prediction?
# We can not give a single answer to this question, because the prediction
# depends on the values of the predictors. We can use the predict() function to
# predict the response variable for a given set of predictor values.

interval_width <- predictions[, "upr"] - predictions[, "lwr"]
mean(interval_width)/2

# We can say we have a variation of +/-3.32 units in the prediction