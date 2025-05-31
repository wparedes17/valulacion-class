library(tidyverse)

credit <- read_csv("./datasets/Credit.csv")
# Remove the first column because it is just an index
# and not a predictor variable
credit <- credit %>%
    select(-`...1`)

# Convert character variables to factors
# This is an efficient way to convert all character variables to factors
credit <- credit %>%
    mutate_if(is.character, as.factor)

# Not efficient way to convert character variables to factors
#credit <- credit %>%
#    mutate(
#        Student = as.factor(Student),
#        Married = as.factor(Married)
#    )

glimpse(credit)

# pairs() function is used to create a matrix of scatterplots
# It is a fast way to visualize the relationships between all the variables in the dataset
# It is a good way to check for multicollinearity
# It is a good way to check for outliers
# It is a good way to check for linearity
# It is a good way to check for normality
# It is a good way to check for homoscedasticity
# It is a good way to check for independence
pairs(credit)


# The most basic prediction model is the average of the response variable
# This is a good starting point to compare with other models
# This is known as the null model

null_model <- mean(credit$Rating)

# histogram of Rating variable using ggplot2
ggplot(credit, aes(x = Rating)) +
    geom_histogram(binwidth = 20, fill = "blue", color = "black") +
    geom_vline(aes(xintercept = null_model), color = "red", linetype = "dashed", size = 1) +
    labs(title = "Histogram of Rating", x = "Rating", y = "Count") +
    theme_minimal()

# In this case, variance is the sum of squares of the deviations from the mean
# In other words, it is the sum of squares of the residuals (the squared errors)
# Variance is the highest possible value of the sum of squares (RSS), so we call it the total sum of squares (TSS)
# R-squared = 1 - (RSS/TSS)

# Scatterplot matrix points to Student variable might be useful to predict the response variable: Rating
fit <- lm(Rating ~ Ethnicity, data = credit)
summary(fit)

# violin plot of Rating variable by Student variable 
credit %>% ggplot(aes(x = Ethnicity, y = Rating)) +
    geom_violin(fill = "blue", color = "black") +
    geom_hline(aes(yintercept = 365.07), color = "red", linetype = "dashed", size = 1) +
    geom_hline(aes(yintercept = 365.07 - 19.64), color = "green", linetype = "dashed", size = 1) +
    geom_hline(aes(yintercept = 365.07 - 10.30), color = "#5500ff", linetype = "dashed", size = 1) +
    labs(title = "Violin plot of Rating by Student", x = "Student", y = "Rating") +
    theme_minimal()



# Scatterplot matrix points to Student variable might be useful to predict the response variable: Rating
fit <- lm(Rating ~ Ethnicity + Income, data = credit)
credit <- credit %>%
    mutate(
        prediction = 3.4728 * Income + 199.4810,
        prediction_2 = 3.4728 * Income + 199.4810 - 7.5045,
        prediction_3 = 3.4728 * Income + 199.4810 + 3.6777
    )
summary(fit)

# violin plot of Rating variable by Student variable 
ggplot(credit, aes(x = Income, y = Rating)) +
    geom_point() +
    geom_line(aes(x = Income, y = prediction), color = "blue", linetype = "dashed", size = 1) +
    geom_line(aes(x = Income, y = prediction_2), color = "green", linetype = "dashed", size = 1) +
    geom_line(aes(x = Income, y = prediction_3), color = "#5500ff", linetype = "dashed", size = 1) +
    labs(title = "Rating using Income + Ethnicity", x = "Income", y = "Rating") +
    theme_minimal()


fit <- lm(Rating ~ Ethnicity * Income, data = credit)
credit <- credit %>%
    mutate(
        prediction = (3.4728) * Income + 189.7606,
        prediction_2 = (3.4728 - 0.1815) * Income + 189.7606 + 1.2285,
        prediction_3 = (3.4728 - 0.3541) * Income + 189.7606 + 17.0853
    )
summary(fit)

# violin plot of Rating variable by Student variable 
ggplot(credit, aes(x = Income, y = Rating)) +
    geom_point() +
    geom_line(aes(x = Income, y = prediction), color = "blue", linetype = "dashed", size = 1) +
    geom_line(aes(x = Income, y = prediction_2), color = "green", linetype = "dashed", size = 1) +
    geom_line(aes(x = Income, y = prediction_3), color = "#5500ff", linetype = "dashed", size = 1) +
    labs(title = "Rating using Income + Ethnicity", x = "Income", y = "Rating") +
    theme_minimal()



fit <- lm(Rating ~ Ethnicity * Student, data = credit)
credit <- credit %>%
    mutate(
        prediction = (3.4728) * Income + 189.7606,
        prediction_2 = (3.4728 - 0.1815) * Income + 189.7606 + 1.2285,
        prediction_3 = (3.4728 - 0.3541) * Income + 189.7606 + 17.0853
    )
summary(fit)


fit <- lm(Rating ~ log(Income) + log(Limit), data = credit)
credit <- credit %>%
    mutate(
        prediction = 41.6 + 0.0661 * Limit - 0.05702 * Income + 0.000009792 * Limit * Income,
    )
summary(fit)

par(mfrow = c(2, 2))
plot(fit)


# First issue
# Non-linearity

# Second issue
# Non - Independence and Non - Normality

# Third issue
# Non - Homoscedasticity

# Fourth issue
# Outliers
fit <- lm(Income ~ Limit + Rating, data = credit, subset = -c(324, 338, 29))
summary(fit)

# Fifth issue
# Leverage points

# Sixth issue
# Multicollinearity

library(car)
vif(fit)
