library(tidyverse)

apple_quality <- read_csv("datasets/apple_quality.csv")
head(apple_quality, n = 10)

# Due to Acidity is a numeric variable but 
# R is reading it as a character, we need to fix it
# We found that there is a row with text in the Acidity column
# We will remove it, also we know that it is the last row
n <- nrow(apple_quality)
apple_quality <- apple_quality[-n, ]

# However, removing the last row is not enough
# We need to convert the Acidity column to numeric
head(apple_quality, n = 10)
apple_quality <- apple_quality %>%
  mutate(Acidity = as.numeric(Acidity))

# Now we can see that Acidity is a numeric variable
head(apple_quality, n = 10)

# A good question is Quality is related to Acidity?
# There are only 2 levels of Quality
apple_quality %>% group_by(Quality) %>% 
  summarise(n = n())

# A t-test is a good way to compare the means of two groups
# Alternative 1
t.test(apple_quality$Acidity ~ apple_quality$Quality)

# Alternative 2
# Select data by groups
apple_good <- apple_quality %>% filter(Quality == "good") %>% select(Acidity)
apple_bad <- apple_quality %>% filter(Quality == "bad") %>% select(Acidity)
# Apply t-test for the groups
t.test(apple_bad, apple_good)

# Correlation between variables
cor(apple_quality$Acidity, apple_quality$Juiciness)

# However, we can use default method for correlation between numeric and factor variables
apple_quality <- apple_quality %>%
  mutate(QualityFactor = as.numeric(factor(Quality, levels = c("bad", "good"))))

# A good alternative is to use the Spearman correlation
cor(apple_quality$Acidity, apple_quality$QualityFactor,
    method = "spearman")
