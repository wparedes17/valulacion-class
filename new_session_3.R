library(tidyverse)

# assume a population of 10,000 subjects
# remember that, in reality, we do not know the population mean
population <- tibble(
    student = 1:10000,
    note = rexp(10000, 6.5)
)

mean_population <- mean(population$note)

ggplot(population, aes(x = note)) +
    geom_histogram(aes(y = after_stat(count/sum(count))), binwidth = 0.01, fill = "blue", color = "black") +
    labs(title = "Histogram of Population Notes", x = "Note", y = "Density") +
    theme_minimal()


# to estimate the population mean, we take samples of 20 subjects
# our budget only allows us to take that sample size
set.seed(123)
sample_of_means <- c()
for (i in 1:1000){
  sample_pop <- sample_n(population, 200)
  mean_sample <- mean(sample_pop$note)
  sample_of_means <- c(sample_of_means, mean_sample)
}
ggplot(sample_pop, aes(x = note)) +
    geom_histogram(aes(y = after_stat(count/sum(count))), binwidth = 0.01, fill = "blue", color = "black") +
    labs(title = "Histogram of Sample Notes", x = "Note", y = "Density") +
    theme_minimal()

ggplot(tibble(note = sample_of_means), aes(x = note)) +
    geom_histogram(aes(y = after_stat(count/sum(count))), binwidth = 0.01, fill = "blue", color = "black") +
    labs(title = "Histogram of Sample Means", x = "Mean", y = "Density") +
    theme_minimal()


# ci for the population mean
t.test(sample_pop$note, conf.level = 0.95)

# ci for population mean was (5.965, 7.765)
# note that for each element in the interval,
# if we perform a t-test assuming that value as H0, we will get a p-value greater than 0.05
# so, we can't reject the null hypothesis
t.test(sample_pop$note, mu = 7.765, alternative = "two.sided")


#####
#
population <- tibble(
    student = 1:10000,
    note = rbinom(10000, 1, 0.6)
)

mean_population <- mean(population$note)

sample_df <- sample_n(population, 200)

n <- 10
pr <- 0.4

binom_df <- tibble(
    p = 0:n,
    likelihood = dbinom(0:n, n, pr),
    normal_likelihood = dnorm(0:n, mean = n*pr, sd = sqrt(n*pr*(1-pr)))
)

# barplot of the likelihood
ggplot(binom_df, aes(x = p, y = likelihood)) +
    geom_col(fill = "blue", color = "black") +
    geom_line(aes(y = normal_likelihood), color = "red") +
    labs(title = "Likelihood of p", x = "p", y = "Likelihood") +
    theme_minimal()

prop.test(sum(sample_df$note), n=nrow(sample_df), p = 0.4, alternative = "two.sided")


power.anova.test(groups = 1, n = 10, between.var = 1, within.var = 1, sig.level = 0.05)

# sigma/sqrt(n)

pnorm(268, mean=270, sd=60/sqrt(480), lower.tail = TRUE)
pnorm(0, mean=1, sd=1/sqrt(9), lower.tail = TRUE)