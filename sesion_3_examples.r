library(ggplot2)
# This generate the first examples of session 3

data_curve_1 <- rgamma(100, shape = 1, rate = 1)
data_curve_2 <- rgamma(100, shape = 2, rate = 0.5)

# Histogram + Density of data_curve_1
histogram_data_curve_1 <- ggplot(data.frame(data_curve_1), aes(x = data_curve_1)) +
    geom_histogram(aes(y=..density..)) + 
    geom_density(color="blue", size=2) +
    xlab("Data curve 1") + ylab("Density")

# Histogram + Density of data_curve_2
histogram_data_curve_2 <- ggplot(data.frame(data_curve_2), aes(x = data_curve_2)) +
    geom_histogram(aes(y=..density..)) + 
    geom_density(color="blue", size=2) +
    xlab("Data curve 2") + ylab("Density")

# Save plots
ggsave("plots/histogram_data_curve_1.png", histogram_data_curve_1, width = 10, height = 10, dpi = 300)
ggsave("plots/histogram_data_curve_2.png", histogram_data_curve_2, width = 10, height = 10, dpi = 300)


# Bimodality example
data_bimodal <- c(rnorm(100, mean = 0, sd = 1), rnorm(100, mean = 5, sd = 1))

# Histogram + Density of data_bimodal
histogram_data_bimodal <- ggplot(data.frame(data_bimodal), aes(x = data_bimodal)) +
    geom_histogram(aes(y=..density..)) + 
    geom_density(color="blue", size=2) +
    xlab("Data bimodal") + ylab("Density")

# Save plot
ggsave("plots/histogram_data_bimodal.png", histogram_data_bimodal, width = 10, height = 10, dpi = 300)


# Skewness example
data_skewness <- rexp(100)

# Histogram + Density of data_skewness
histogram_data_skewness <- ggplot(data.frame(data_skewness), aes(x = data_skewness)) +
    geom_histogram(aes(y=..density..)) + 
    geom_density(color="blue", size=2) +
    xlab("Data skewness") + ylab("Density")

# Save plot
ggsave("plots/histogram_data_skewness.png", histogram_data_skewness, width = 10, height = 10, dpi = 300)
