library(tidyverse)

# Read the data (change the path to your local path if it is necessary)
boston_housing <- read_csv("datasets/BostonHousing.csv")

# Show the data
glimpse(boston_housing)

# Here we assign the two samples of crim variable according to the variable chas
# Assign sample A
sample_a <- boston_housing %>% filter(chas == 0) %>% pull(crim)

# Assign sample B 
sample_b <- boston_housing %>% filter(chas == 1) %>% pull(crim)
z
# t.test
print(t.test(sample_a, sample_b, var.equal = FALSE))

# Write my conclusion
# p.value = 0.001636. Therefore, it is not feasible null hypothesis. So, 
# we should reject the null hypothesis. In other words, we have enough evidence
# to say that the crime rate is different between the two samples.