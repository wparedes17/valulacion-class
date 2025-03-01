library(tidyverse)

# Load the data
diamonds <- read_csv("datasets/diamonds - diamonds.csv")
#preview the data
head(diamonds)

# This is the reality of the data
carat_hist <- ggplot(diamonds) +
  geom_histogram(aes(x = log(carat), after_stat(density)), binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Histogram of Carat", x = "Carat", y = "Frequency") +
  theme_minimal()

carat_hist

# We are going to assume that the data is normally distributed
mu <- mean(log(diamonds$carat))
sigma <- sd(log(diamonds$carat))

# Generate a normal distribution
carat_hist +
  stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), color = "red")

# For example, under the normal distribution,
# what is the probability of a diamond being larger than 1 carat?

# Due to 1 carat being the same as log(1) = 0
# we can use the pnorm function to calculate the probability
# using the computed mean and standard deviation

# lower.tail = FALSE is used to calculate the probability of being larger than 1 carat
# lower.tail = TRUE is used to calculate the probability of being smaller than 1 carat
model_probability <- pnorm(0, mean = mu, sd = sigma, lower.tail = FALSE)

# Check the probability under the model against the reality
# We can see that the model is not perfect
data_probability <- diamonds %>%
  filter(carat > 1) %>%
  nrow() / nrow(diamonds)
# model = 24.97%, data = 32.44%


# The mean is also random when we have population data
# In this example we are going to consider population involves 10 student notes
population <- tibble(
    student = 1:10,
    note = c(8.2, 6.2, 8.0, 5.7, 7.2, 6.8, 7.0, 6.5, 7.0, 6.5)
)

mu_population <- mean(population$note)
sd_population <- sd(population$note)

# Sample of size 4 from the population

mu_samples <- c()
sd_samples <- c()

for (i in 1:1000) {
  sample <- population %>%
    sample_n(4)

  mu_sample <- mean(sample$note)
  sd_sample <- sd(sample$note)

  mu_samples <- c(mu_samples, mu_sample)
  sd_samples <- c(sd_samples, sd_sample)
}

sample_df <- tibble(
  mu = mu_samples,
  sd = sd_samples
)

# Histogram of the sample means
sample_hist <- ggplot(sample_df) +
  geom_histogram(aes(x = mu, after_stat(density)), binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "Histogram of Sample Means", x = "Mean", y = "Frequency") +
  theme_minimal()

sample_hist

mu <- mean(sample_df$mu)
sigma <- sd(sample_df$mu)

sample_hist +
  stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), color = "red")

# Histogram of sd
sample_hist_sd <- ggplot(sample_df) +
  geom_histogram(aes(x = sd, after_stat(density)), binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "Histogram of Sample Standard Deviations", x = "Standard Deviation", y = "Frequency") +
  theme_minimal()

sample_hist_sd

mu <- mean(sample_df$sd)
sigma <- sd(sample_df$sd)

sample_hist_sd +
  stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), color = "red")

sd_population/sqrt(4)


statistic <- abs((7 - 6.75)/(0.331662/2))
pnorm(statistic, lower.tail = FALSE)
pt(statistic, df = 3, lower.tail = FALSE)
pnorm(7, mean = mu, sd = sigma, lower.tail = FALSE)

# Another example
wine <- tibble(
    person = 1:10,
    threshold = c(31, 31, 43, 36, 23, 34, 32, 30, 20, 24)
)

mu <- mean(wine$threshold)
sigma <- sd(wine$threshold)

# The threshold is 25
statistic <- abs((25 - mu)/(sigma/sqrt(10)))
pnorm(statistic, lower.tail = FALSE)
pt(statistic, df = 9, lower.tail = FALSE)

t.test(wine$threshold, mu = 25, alternative = "greater")

# Due to the p-value being less than 0.05, we reject the null hypothesis
# null hypothesis: the threshold is 25 or less
# alternative hypothesis: the threshold is greater than 25
# reject the null hypothesis means that the threshold is greater than 25

# Note, p-value less than 0.05 is a common threshold for rejecting the null hypothesis
# However, it is not a strict rule, and it is up to the researcher to decide the threshold

# for less hypothesis, we can use the less argument or lower.tail = TRUE
statistic <- abs((25 - mu)/(sigma/sqrt(10)))
pnorm(statistic, lower.tail = TRUE)
pt(statistic, df = 9, lower.tail = TRUE)

t.test(wine$threshold, mu = 25, alternative = "less")

# for two-tailed hypothesis, we can use the two-sided argument or twice the p-value for lower.tail = FALSE
2*pnorm(statistic, lower.tail = TRUE)
2*pt(statistic, df = 9, lower.tail = FALSE)
t.test(wine$threshold, mu = 25, alternative = "two.sided")

## Iphone example:
iphone_sample <- tibble(
    iphone_no = 1:60,
    price = c(
      6727, 6625, 5178, 6910, 9581,
      5300, 2599, 4484, 5300, 5096,
      6931, 2966, 8358, 4056, 6971,
      6727, 6625, 5178, 6910, 9581,
      5300, 2599, 4484, 5300, 5096,
      6931, 2966, 8358, 4056, 6971,
      6727, 6625, 5178, 6910, 9581,
      5300, 2599, 4484, 5300, 5096,
      6931, 2966, 8358, 4056, 6971,
      6727, 6625, 5178, 6910, 9581,
      5300, 2599, 4484, 5300, 5096,
      6931, 2966, 8358, 4056, 6971)
)
n <- nrow(iphone_sample)
mu_0 <- 5500
mu_sample <- mean(iphone_sample$price)

statistic <- (mu_sample - mu_0)/(sd(iphone_sample$price)/sqrt(n))

# Less hypothesis
pnorm(statistic, lower.tail = TRUE) # when i know sigma or n is large
pt(statistic, df = n-1, lower.tail = TRUE) # when i don't know sigma or n is small
t.test(iphone_sample$price, mu = 5500, alternative = "less")

# Greater hypothesis
pnorm(statistic, lower.tail = FALSE) # when i know sigma or n is large
pt(statistic, df = n-1, lower.tail = FALSE) # when i don't know sigma or n is small
t.test(iphone_sample$price, mu = 5500, alternative = "greater")



# IQ example
iq_sample <- tibble(
    iq = c(
      114, 100, 104, 89, 102, 91, 114, 114, 103, 105,
      108, 130, 120, 132, 111, 128, 118, 119, 86, 72,
      111, 103, 74, 112, 107, 103, 98, 96, 112, 112, 93)
    )
n <- nrow(iq_sample)
mu_0 <- 100
mu_sample <- mean(iq_sample$iq)

statistic <- (mu_sample - mu_0)/(sd(iq_sample$iq)/sqrt(n))

# Different or two.sided hypothesis
pnorm(statistic, lower.tail = FALSE)*2
pt(statistic, df = n-1, lower.tail = FALSE)*2
t.test(iq_sample$iq, mu = mu_0, alternative = "two.sided")

# p-value = 0.03003, this means data doesnt support the null hypothesis.
# Thus, we can claim we have enough evidence to reject the null hypothesis and support the alternative hypothesis.
# In this, case data supports the alternative hypothesis that the average IQ is different from 100

# When we use significance levels, i.e, alpha = 0.05, we can reject the null hypothesis.
# However, considering alpha = 0.01 we can not reject the null hypothesis.
