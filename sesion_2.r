# This a simulated example to show bias effect

# Tools for plotting
library(ggplot2)

# Here we simulate salary of a population of 1000 people
salaries <- rnorm(1000, mean = 12000, sd = 1500)

# We take a sample of 100 people
# Here all the people have the same probability to be selected
# We choose the ids of the individuals
ids <- sample(1:1000, 100)
# We select the individuals with the ids
sample_salaries <- salaries[ids]

# This is more efficient, because we do not need to create the ids
# Software only select the individuals from the population
#sample_salaries <- sample(salaries, 100)

# Density plot of the population and the sample
density_salaries <- ggplot() +
    geom_density(aes(x = salaries), color = "blue", size = 2) +
    geom_density(aes(x = sample_salaries), color = "red", size = 2) +
    xlab("Salary") + ylab("Density")

# Save plot
ggsave("plots/density_salaries.png", density_salaries, width = 10, height = 10, dpi = 300)

# However sometimes bias conditions can be present
# For example, people with salary below than 12000 has less probability to say how much they earn than people with salary above 12000
# In this case, we select the next indivial to substitute the previous one if he/she does not want to say how much he/she earns

# This help to simulate who say his/her salary
say_his_salary <- function(salary) {
    if (salary < 12000 && runif(1) < 0.3){
        return (1)
    } else if (salary < 12000 && runif(1) >= 0.3){
        return (0)
    } else if (salary >= 12000 && runif(1) < 0.7){
        return (1)
    }
    return (0)
}

# We simulate the people who say his/her salary
yes <- sapply(salaries, say_his_salary)

# We choose the ids of the individuals
ids <- sample(1:1000, 100)

# We measure who say his/her salary
yes[ids]

# Count the number of people who don't say his/her salary
n <- sum(yes[ids] == 0)

# We substitute the people who don't say his/her salary by the next one
while (n > 0){
    ids[which(yes[ids] == 0)] = ids[which(yes[ids] == 0)] + 1
    n <- sum(yes[ids] == 0)
}

sample_salaries <- salaries[ids]


# Density plot of the population and the sample
density_salaries <- ggplot() +
    geom_density(aes(x = salaries), color = "blue", size = 2) +
    geom_density(aes(x = sample_salaries), color = "red", size = 2) +
    xlab("Salary") + ylab("Density")

density_salaries
ggsave("plots/density_salaries_2.png", density_salaries, width = 10, height = 10, dpi = 300)
