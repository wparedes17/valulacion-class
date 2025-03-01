library(tidyverse)

n <- 1500
pr <- 0.4
theorical_binom_df <- tibble(
    x = 0:n,
    p = dbinom(0:n, size = n, prob = pr),
    n = dnorm(0:n, mean = n*pr, sd = sqrt(n*pr*(1-pr)))
)

ggplot(theorical_binom_df, aes(x = x, y = p)) +
    geom_point() +
    geom_line() +
    geom_line(aes(y = n), color = "red") +
    labs(title = "Theorical Binomial Distribution", x = "x", y = "P(x)") +
    theme_minimal()

n <- 15
pr <- 0.4
surveys <- tibble(
  subject = 1:n,
    result = rbinom(n, 1, pr)
)

prop.test(
    sum(surveys$result),
    n = nrow(surveys),
    p = pr,
    alternative = "two.sided"
)


n <- 15
pr <- 0.4
surveys_1 <- tibble(
  subject = 1:n,
    result = rbinom(n, 1, pr)
)


n <- 20
pr <- 0.7
surveys_2 <- tibble(
  subject = 1:n,
    result = rbinom(n, 1, pr)
)

n1 <- nrow(surveys_1)
p1_est <- mean(surveys_1$result)
sigma_p1 <- sqrt(p1_est*(1-p1_est)/n1)

n2 <- nrow(surveys_2)
p2_est <- mean(surveys_2$result)
sigma_p2 <- sqrt(p2_est*(1-p2_est)/n2)

p_diff <- p1_est - p2_est
sigma_ps <- sqrt(sigma_p1^2 + sigma_p2^2)

alpha <- 0.05
ci_lower <- p_diff - qnorm(1-alpha/2)*sigma_ps
ci_upper <- p_diff + qnorm(1-alpha/2)*sigma_ps

print(paste("CI: (", ci_lower, ", ", ci_upper, ")"))
