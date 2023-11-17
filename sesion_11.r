# This is how to use principal component analysis
# PCA only works on numeric data.
# PCA is a way to reduce the dimensionality of your data set.
# PCA is a way to find the most important parts of your data.
# PCA is a way to find the most important variables in your data.
# PCA is a way to find the most important features in your data.

library(tidyverse)
library(ordr)
library(cowplot)

boston_housing <- read.csv("datasets/BostonHousing.csv")

# Boston Housing includes CHAS variable which is a categorical variable
# However, in the dataset, it is coded as 0 and 1
# So, if we run PCA on this dataset, it will treat CHAS as a numeric variable
# And no error will be thrown. But the results will be wrong.
# So, we need to convert CHAS to a factor variable 
# and exclude it from the PCA analysis.

# Casting CHAS as a factor variable
boston_housing <- boston_housing %>% 
  mutate(chas = as.factor(chas))

# Excluding CHAS from the PCA analysis
boston_housing_pca_data <- boston_housing %>% 
  select(-chas)

# When we try to understand the relationship between 
# predictor variables and the response variable,
# removing the response variable from the dataset is a good idea.
# So, we'll remove medv from the dataset.
boston_housing_pca_data <- boston_housing_pca_data %>% 
  select(-medv)

# PCA is magnitude sensitive
# So, we need to scale the data before running PCA
# Scaling the data
boston_housing_pca_data <- boston_housing_pca_data %>% 
  scale()

# Running PCA
boston_housing_pca_fit <- princomp(boston_housing_pca_data, 
    cor = TRUE,
    scores = TRUE)

boston_housing_pca_data

# Exploring the results
# Summary gives the proportion of variance explained 
# by each principal component. We'll get the same 
# amount of components as the number of variables 
# In this case, 13 components.
# Components are ordered by the amount of variance explained.
# So, the idea is choose the first components. 
# That's why PCA is used for dimensionality reduction.
# We reduce the number of variables by choosing the first few components.
# As a rule of thumb, we choose the components that explain 70% of the variance.
# In this case, the first 3 components explain 72.34% of the variance.
summary(boston_housing_pca_fit)

# Scree plot is a way to visualize the amount of variance explained by each component.
component_variance <- diag(var(boston_housing_pca_fit$scores))
component_variance <- component_variance/sum(component_variance)
component_variance <- tibble(component = 1:13, 
    variance = component_variance)

scree_plot <- component_variance %>% 
  ggplot(aes(x = component, y = variance)) +
  geom_bar(stat = "identity") +
  geom_line() +
  theme_bw()

scree_plot

boston_housing_pca_fit$loadings[, 1:3]

# Biplot is a way to visualize the relationship between the variables and the components.
component_representation <- as.data.frame(boston_housing_pca_fit$loadings[1:12, 1:12])

biplot_12 <- component_representation %>% 
  ggplot(aes(x = Comp.1, y = Comp.2)) +
  geom_vector() + 
  geom_text_repel(aes(label = rownames(component_representation))) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dashed') +
  geom_hline(yintercept = 0, color = "red", linetype = 'dashed') 
  
biplot_13 <- component_representation %>% 
  ggplot(aes(x = Comp.1, y = Comp.3)) +
  geom_vector() + 
  geom_text_repel(aes(label = rownames(component_representation))) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dashed') +
  geom_hline(yintercept = 0, color = "red", linetype = 'dashed') 
  
biplot_23 <- component_representation %>% 
  ggplot(aes(x = Comp.2, y = Comp.3)) +
  geom_vector() + 
  geom_text_repel(aes(label = rownames(component_representation))) +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dashed') +
  geom_hline(yintercept = 0, color = "red", linetype = 'dashed') 
  
biplot <- ggdraw() +
    draw_plot(biplot_12, x = 0, y = 0, width = 0.3, height = 1) +
    draw_plot(biplot_13, x = 0.33, y = 0, width = 0.3, height = 1) +
    draw_plot(biplot_23, x = 0.66, y = 0, width = 0.3, height = 1) 

biplot

# Individual plots
# We'll create individual plots for the first 3 components
scores <- as.data.frame(boston_housing_pca_fit$scores)

scores_12 <- scores %>% 
  ggplot(aes(x = Comp.1, y = Comp.2)) +
  geom_point() +
  theme_bw()

scores_13 <- scores %>% 
  ggplot(aes(x = Comp.1, y = Comp.3)) +
  geom_point() +
  theme_bw()

scores_23 <- scores %>% 
  ggplot(aes(x = Comp.2, y = Comp.3)) +
  geom_point() +
  theme_bw()

# Combining the plots
scores_plots <- ggdraw() +
    draw_plot(scores_12, x = 0, y = 0, width = 0.3, height = 1) +
    draw_plot(scores_13, x = 0.33, y = 0, width = 0.3, height = 1) +
    draw_plot(scores_23, x = 0.66, y = 0, width = 0.3, height = 1)

scores_plots

# We can also visualize categorical variables in the individual plots
# We'll use the CHAS variable

# First we need to add CHAS to the scores dataset
scores_chas <- scores %>% 
  mutate(chas = boston_housing$chas) 

# Compute mean of the scores for each category of CHAS
scores_chas_mean <- scores_chas %>% 
  group_by(chas) %>% 
  summarise_all(mean)

# Generate a tag to use colors
scores <- scores %>% mutate(
    observation_type = 'individual'
)

# Generate tags for the mean scores
scores_chas_mean <- scores_chas_mean %>% mutate(
    observation_type = c('chas = 0', 'chas = 1')
)

# Combine the two datasets
scores_combined <- bind_rows(scores, scores_chas_mean)

# Repeat the plots using group = observation_type
scores_12 <- scores_combined %>% 
  ggplot(aes(x = Comp.1, y = Comp.2, color = observation_type)) +
  geom_point() +
  theme_bw()

scores_13 <- scores_combined %>% 
  ggplot(aes(x = Comp.1, y = Comp.3, color = observation_type)) +
  geom_point() +
  theme_bw()

scores_23 <- scores_combined %>% 
  ggplot(aes(x = Comp.2, y = Comp.3, color = observation_type)) +
  geom_point() +
  theme_bw()

# Combining the plots
scores_plots <- ggdraw() +
    draw_plot(scores_12, x = 0, y = 0, width = 0.3, height = 1) +
    draw_plot(scores_13, x = 0.33, y = 0, width = 0.3, height = 1) +
    draw_plot(scores_23, x = 0.66, y = 0, width = 0.3, height = 1)

scores_plots
