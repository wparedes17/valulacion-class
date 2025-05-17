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

# Scatterplot matrix points to Student variable might be useful to predict the response variable: Rating
fit <- lm(Rating ~ Student + Limit, data = credit)
summary(fit)
