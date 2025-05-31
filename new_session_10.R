library(tidyverse)

boston_housing <- read_csv("./datasets/BostonHousing.csv")
boston_housing_scaled <- as_tibble(scale(boston_housing))
boston_no_chas <- boston_housing_scaled %>%
    select(-chas, -medv)

glimpse(boston_no_chas)




fit_pca <- princomp(boston_no_chas, cor = TRUE, scores = TRUE)
summary(fit_pca)

pairs(fit_pca$scores)
fit_pca$loadings

# Plot the first two principal components
boston_no_chas %>%
    ggplot(aes(x = fit_pca$scores[, 1], y = fit_pca$scores[, 2])) +
    geom_point() +
    labs(title = "Principal Component Analysis of Boston Housing Data",
         x = "Principal Component 1",
         y = "Principal Component 2") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    theme_minimal()


pca_data <- as_tibble(fit_pca$scores[, 1:3])
pca_data <- pca_data %>%
    mutate(medv = boston_housing_scaled$medv)

fit_lm <- lm(medv ~ `Comp.1`*cluster + `Comp.2` + poly(`Comp.3`,2), data = pca_data)
summary(fit_lm)
mfrow <- c(2, 2)
par(mfrow = mfrow)
plot(fit_lm)

pairs(pca_data)

# k means clustering, 2 clusters for v1 and v2 in pca_data
set.seed(123)
kmeans_result <- kmeans(pca_data[, 1:2], centers = 2, nstart = 20)
pca_data <- pca_data %>%
    mutate(cluster = as.factor(kmeans_result$cluster))

pca_data %>%
    ggplot(aes(x = `Comp.1`, y = `Comp.2`, color = cluster)) +
    geom_point() +
    labs(title = "K-means Clustering on PCA Data",
         x = "Principal Component 1",
         y = "Principal Component 2") +
    theme_minimal() +
    scale_color_manual(values = c("red", "blue")) +
    theme(legend.title = element_blank())

