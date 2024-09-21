library(tidyverse)

california_housing <- read_csv('datasets/california_housing.csv')

# Convert character variables to factors (an explicit categorical variable)
california_housing <- california_housing %>% 
    mutate(ocean_proximity = as.factor(ocean_proximity))

# An alternative way to convert character variables to factors
#california_housing$ocean_proximity <- as.factor(california_housing$ocean_proximity)
glimpse(california_housing)

# Fast summary of the dataset
summary(california_housing)

# We realized that median house value of 500001 might be a NAN indicator
# We are going to remove the rows with median house value of 500001
# Also, we are going to remove the rows with missing values in the total bedrooms
california_housing <- california_housing %>%
    filter(median_house_value != 500001) %>%
    filter(!is.na(total_bedrooms))

# We are going to explore the relationship between the median house value and the ocean proximity
# In a first step, we are going to compute the summary statistics of the median house value by ocean proximity
ocean_proximity_summary_median_house <- california_housing %>%
    group_by(ocean_proximity) %>%
        summarise(
            min_median_house_value = min(median_house_value),
            fq_median_house_value = quantile(median_house_value, 0.25),
            median_median_house_value = quantile(median_house_value, 0.50),
            mean_median_median_house_value = mean(median_house_value),
            tq_median_median_house_value = quantile(median_house_value, 0.75),
            max_median_house_value = max(median_house_value)
        )

# it seems that median house value is lower for inland houses
# we are going to group the variables in only two categories: inland and not inland
california_housing <- california_housing %>%
    mutate(inland = ifelse(ocean_proximity == 'INLAND', 'INLAND', 'NOT INLAND'))

# histogram of median house value by inland
california_housing %>%
    filter(inland == 'INLAND') %>%
    ggplot(aes(x = median_house_value)) +
        geom_histogram(bins = 30, alpha = 0.5) +
        labs(title = 'Histogram of median house value for inland houses',
             x = 'Median house value',
             y = 'Frequency') +
        theme_minimal()

california_housing %>%
    filter(inland == 'NOT INLAND') %>%
    ggplot(aes(x = median_house_value)) +
        geom_histogram(bins = 30, alpha = 0.5) +
        labs(title = 'Histogram of median house value for inland houses',
             x = 'Median house value',
             y = 'Frequency') +
        theme_minimal()

# Histograms show a biassed distribution of median house value, we are going to log transform the variable
california_housing <- california_housing %>%
    mutate(log_median_house_value = log(median_house_value))

california_housing %>%
    filter(inland == 'INLAND') %>%
    ggplot(aes(x = log_median_house_value)) +
        geom_histogram(bins = 30, alpha = 0.5) +
        labs(title = 'Histogram of median house value for inland houses',
             x = 'Median house value',
             y = 'Frequency') +
        theme_minimal()

california_housing %>%
    filter(inland == 'NOT INLAND') %>%
    ggplot(aes(x = log_median_house_value)) +
        geom_histogram(bins = 30, alpha = 0.5) +
        labs(title = 'Histogram of median house value for inland houses',
             x = 'Median house value',
             y = 'Frequency') +
        theme_minimal()

california_housing <- california_housing %>%
    mutate(cheap_zones = ifelse(log_median_house_value < 10.5, 'CHEAP', 'NOT CHEAP'))

california_housing %>%
    filter((inland == 'INLAND') & (cheap_zones == 'NOT CHEAP')) %>%
    ggplot(aes(x = log_median_house_value)) +
        geom_histogram(bins = 30, alpha = 0.5) +
        labs(title = 'Histogram of median house value for inland houses',
             x = 'Median house value',
             y = 'Frequency') +
        theme_minimal()

california_housing %>%
    filter((inland == 'NOT INLAND') & (cheap_zones == 'NOT CHEAP')) %>%
    ggplot(aes(x = log_median_house_value)) +
        geom_histogram(bins = 30, alpha = 0.5) +
        labs(title = 'Histogram of median house value for inland houses',
             x = 'Median house value',
             y = 'Frequency') +
        theme_minimal()

# We are going to test the hypothesis that the median house value is different between inland and not inland houses
t.test(log_median_house_value ~ inland, 
    data = california_housing, 
    subset = (cheap_zones == 'NOT CHEAP'))

# Due to the p-value of the t-test, 
# we reject the null hypothesis that the median house value is the same for inland and not inland houses
# So, we conclude that inland variable is related to the median house value

# Boxplot of the median house value by ocean proximity
california_housing %>%
    ggplot(aes(x = ocean_proximity, y = log_median_house_value)) +
        geom_boxplot() +
        labs(title = 'Boxplot of median house value by inland',
             x = 'Ocean proximity',
             y = 'Log median house value') +
        theme_minimal()

# Violin plot of the median house value by ocean proximity
california_housing %>%
    ggplot(aes(x = ocean_proximity, y = log_median_house_value)) +
        geom_violin() +
        labs(title = 'Violin plot of median house value by inland',
             x = 'Ocean proximity',
             y = 'Log median house value') +
        theme_minimal()


#########################################
# Scatter plot of the log median house value by population
california_housing %>%
    ggplot(aes(x = population, y = log_median_house_value)) +
        geom_point(alpha = 0.5) +
        labs(title = 'Scatter plot of log median house value by population',
             x = 'Population',
             y = 'Log median house value') +
        theme_minimal()

# compute the correlation between the log median house value and the population
cor(california_housing$population, california_housing$log_median_house_value)
cor.test(california_housing$population, california_housing$log_median_house_value)


# Scatter plot of the log median house value by median income
california_housing %>%
    ggplot(aes(x = median_income, y = median_house_value)) +
        geom_point(alpha = 0.5) +
        labs(title = 'Scatter plot of log median house value by population',
             x = 'Median Income',
             y = 'Log median house value') +
        theme_minimal()

# compute the correlation between the log median house value and the median income
cor(california_housing$median_income, california_housing$median_house_value)
cor.test(california_housing$median_income, california_housing$median_house_value)


# Scatter plot of total rooms by median income
california_housing %>%
    ggplot(aes(x = median_income, y = total_rooms)) +
        geom_point(alpha = 0.5) +
        labs(title = 'Scatter plot of log median house value by population',
             x = 'Median Income',
             y = 'Total Rooms') +
        theme_minimal()

# compute the correlation between total rooms and the median income
cor(california_housing$total_rooms, california_housing$median_income)
cor.test(california_housing$total_rooms, california_housing$median_income)


# Scatter plot of ratio_rooms_bedrooms by median income
# First, we are going to compute the ratio between total rooms and total bedrooms
california_housing <- california_housing %>%
    mutate(ratio_rooms_bedrooms = total_rooms / total_bedrooms)

# Then, we are going to plot the scatter plot
california_housing %>%
    ggplot(aes(x = median_income, y = ratio_rooms_bedrooms)) +
        geom_point(alpha = 0.5) +
        labs(title = 'Scatter plot of log median house value by population',
             x = 'Median Income',
             y = 'Total Rooms') +
        theme_minimal()

# compute the correlation between ratio_rooms_bedrooms and the median income
cor(california_housing$ratio_rooms_bedrooms, california_housing$median_income)
cor.test(california_housing$ratio_rooms_bedrooms, california_housing$median_income)

# Sometimes, when a two predictors are correlated, both can be related to the response variable
# We are going to explore the relationship between the ratio_rooms_bedrooms and the log median house value
# Scatter plot of ratio_rooms_bedrooms by median income
california_housing %>%
    ggplot(aes(x = ratio_rooms_bedrooms, y = log_median_house_value)) +
        geom_point(alpha = 0.5) +
        labs(title = 'Scatter plot of log median house value by population',
             x = 'Median Income',
             y = 'Total Rooms') +
        theme_minimal()

# compute the correlation between total rooms and the median income
cor(california_housing$ratio_rooms_bedrooms, california_housing$log_median_house_value)
cor.test(california_housing$ratio_rooms_bedrooms, california_housing$log_median_house_value)
