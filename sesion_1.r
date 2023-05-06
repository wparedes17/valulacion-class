library(tidyverse)

diamonds <- read_csv("datasets/diamonds - diamonds.csv")
glimpse(diamonds)

# Set variable cut as factor
diamonds$cut <- factor(diamonds$cut, 
    levels = c("Fair", "Good", "Very Good", "Premium", "Ideal"))

# Barplot of variable cut
barplot_cut <- ggplot(diamonds, aes(x = cut)) +
    geom_bar()

# Save plot
ggsave("plots/barplot_cut.png", barplot_cut, width = 10, height = 10, dpi = 300)

# Histogram + Density of variable price
histogram_price <- ggplot(diamonds, aes(x = price)) +
    geom_histogram(aes(y=..density..)) + 
    geom_density(color="blue", size=2) +
    xlab("Price") + ylab("Density")

# Save plot
ggsave("plots/histogram_price.png", histogram_price, width = 10, height = 10, dpi = 300)