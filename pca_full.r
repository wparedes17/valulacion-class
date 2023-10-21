library(tidyverse)

# this is only to show how to filter numeric data
diamonds <- read_csv("datasets/diamonds - diamonds.csv")
# this is a way to select all numeric variables
diamonds_numeric <- diamonds %>% 
    select_if(is.numeric)
diamonds_numeric <- diamonds_numeric %>% 
    select(-`...1`, -x, -y, -z)

# load data
boston_housing <- read_csv("datasets/BostonHousing.csv")

# remove chas variable
#boston_housing <- boston_housing %>% 
#    select(-chas)

# our target variable is crim
# histogram of crim
boston_housing %>% 
    ggplot(aes(crim)) +
    geom_histogram()

# there are bias, we are goin to apply log transformation
boston_housing <- boston_housing %>% 
    mutate(crim = log(crim))

# histogram of crim
# there are bi-modal distribution
boston_housing %>% 
    ggplot(aes(crim)) +
    geom_histogram()


# scatter plot of crim vs indus
boston_housing %>% 
    ggplot(aes(indus, crim)) +
    geom_point()

# we are going to create a new categorical variable
# based on the value of indus
# we are going to create a new variable called indus_cat
boston_housing <- boston_housing %>% 
    mutate(indus_cat = case_when(
        indus < 15 ~ "A",
        indus >= 15 ~ "B",
    ))

boston_housing %>%
    ggplot(aes(indus, crim)) +
    geom_point() +
    facet_wrap(~indus_cat)

boston_housing %>% 
    group_by(indus_cat) %>%
    summarise(correlation = cor(crim, indus))


boston_housing_a <- boston_housing %>% 
    filter(indus_cat == "A") %>% 
    select(-indus_cat, -chas)

boston_housing_b <- boston_housing %>% 
    filter(indus_cat == "B") %>% 
    select(-indus_cat, -chas)

crim_a <- boston_housing_a$crim
crim_b <- boston_housing_b$crim
boston_housing_a <- boston_housing_a %>% 
    select(-crim)
boston_housing_b <- boston_housing_b %>%
    select(-crim)

# we are going to apply PCA to boston_housing_a
pca_a <- princomp(boston_housing_a, 
    cor = TRUE, 
    scores=TRUE)

summary(pca_a)

round(pca_a$loadings[,1:4], 2)

pca_a_table <- as.data.frame(pca_a$scores[,1:4])
pca_a_table <- cbind(pca_a_table, crim_a) 
pca_a_table

# scatter plot of PC1 vs crim
pca_a_table %>% 
    ggplot(aes(`Comp.1`, crim_a)) +
    geom_point()

cor(pca_a_table$`Comp.4`, pca_a_table$crim_a)

pca_a_table %>% 
    ggplot(aes(`Comp.2`, crim_a)) +
    geom_point()
