library(ggplot2)

# Data is random, set a seed to reproduce results
set.seed(1234)

# This generate a sample of each case in the session 5
data_curve_1 <- rgamma(100, shape = 1, rate = 1)
data_curve_2 <- rgamma(100, shape = 4, rate = 2)

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


# Compute mean and variance of both curves
statistics_curve_1 <- c(mean(data_curve_1),var(data_curve_1))
statistics_curve_2 <- c(mean(data_curve_2),var(data_curve_2))

print(statistics_curve_1)
print(statistics_curve_2)


# Now we are going to generate a sample of samples
# To each sample we store the mean

sample_means_curve_1 <- sapply(1:100, function(x) mean(rgamma(100, shape = 1, rate = 1)))
sample_means_curve_2 <- sapply(1:100, function(x) mean(rgamma(100, shape = 4, rate = 2)))

# We generate their plots

# Histogram + Density of data_curve_1
histogram_means_curve_1 <- ggplot(data.frame(sample_means_curve_1), aes(x = sample_means_curve_1)) +
    geom_histogram(aes(y=..density..)) + 
    geom_density(color="blue", size=2) +
    xlab("Data curve 1") + ylab("Density")

# Histogram + Density of data_curve_2
histogram_means_curve_2 <- ggplot(data.frame(sample_means_curve_2), aes(x = sample_means_curve_2)) +
    geom_histogram(aes(y=..density..)) + 
    geom_density(color="blue", size=2) +
    xlab("Data curve 2") + ylab("Density")

# Save plots
ggsave("plots/histogram_means_curve_1.png", histogram_means_curve_1, width = 10, height = 10, dpi = 300)
ggsave("plots/histogram_means_curve_2.png", histogram_means_curve_2, width = 10, height = 10, dpi = 300)


# p-valor 
pnorm(8.5933, lower.tail = FALSE)

# t.test
t.test(data_curve_1, data_curve_2, var.equal = TRUE)
