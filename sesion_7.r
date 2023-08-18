# Relationship between numeric variables
# Only plotting this session
library(ggplot2)
library(tibble)
library(dplyr)

# Create a data frame
df <- tibble(predictor = rnorm(100))

# Case 1: Plotting a single line
df <- df %>% mutate(
    line_pattern = 5 * predictor + 3 + rnorm(100)
)

line_plot <- df %>% 
    ggplot(aes(x = predictor, y = line_pattern)) +
    geom_point()

ggsave("plots/line_plot.png", line_plot, width = 10, height = 10)
line_plot

# Case 2: Non linear relationship
df <- df %>% mutate(
    non_linear = predictor^3 + 3 + rnorm(100)
)

non_linear_plot <- df %>% 
    ggplot(aes(x = predictor, y = non_linear)) +
    geom_point()

ggsave("plots/non_linear_plot.png", non_linear_plot, width = 10, height = 10)
non_linear_plot

# Case 3: Another non linear relationship
df <- df %>% mutate(
    non_linear_2 = sin(3*predictor) + rnorm(100, sd=0.2)
)

non_linear_plot_2 <- df %>% 
    ggplot(aes(x = predictor, y = non_linear_2)) +
    geom_point()

ggsave("plots/non_linear_plot_2.png", non_linear_plot_2, width = 10, height = 10)
non_linear_plot_2

# Case 4: Non relationship
df <- df %>% mutate(
    non_relationship = rnorm(100)
)

non_relationship_plot <- df %>% 
    ggplot(aes(x = predictor, y = non_relationship)) +
    geom_point()

ggsave("plots/non_relationship_plot.png", non_relationship_plot, width = 10, height = 10)
non_relationship_plot

# Measuring the relationship using correlation
# Correlantion varies between -1 and 1
correlations_pearson <- df %>% summarise(
        cor_line = cor(predictor, line_pattern, method="pearson"),
        cor_non_linear = cor(predictor, non_linear, method="pearson"),
        cor_non_linear_2 = cor(predictor, non_linear_2, method="pearson"),
        cor_non_relationship = cor(predictor, non_relationship, method="pearson"),
    )

names(correlations_pearson) <- c("Line", "Non linear", "Non linear 2", "Non relationship")
correlations_pearson

# Correlation using Spearman
correlations_spearman <- df %>% 
    summarise(
        cor_line = cor(predictor, line_pattern, method="spearman"),
        cor_non_linear = cor(predictor, non_linear, method="spearman"),
        cor_non_linear_2 = cor(predictor, non_linear_2, method="spearman"),
        cor_non_relationship = cor(predictor, non_relationship, method="spearman")
    )

names(correlations_spearman) <- c("Line", "Non linear", "Non linear 2", "Non relationship")
correlations_spearman

# Alternative way to compute correlations
cor(df)

# Adding hypothesis testing to correlations using Pearson
# H0: No hay correlacion (r = 0) vs H1: Hay correlacion (r != 1)
correlations_pearson <- df %>% 
    summarise(
        cor_line_min = cor.test(predictor, line_pattern, method="pearson")$conf.int[1],
        cor_line = cor.test(predictor, line_pattern, method="pearson")$estimate,
        cor_line_max = cor.test(predictor, line_pattern, method="pearson")$conf.int[2],
        cor_line_pvalue = cor.test(predictor, line_pattern, method="pearson")$p.value,
        cor_non_linear_min = cor.test(predictor, non_linear, method="pearson")$conf.int[1],
        cor_non_linear = cor.test(predictor, non_linear, method="pearson")$estimate,
        cor_non_linear_max = cor.test(predictor, non_linear, method="pearson")$conf.int[2],
        cor_non_linear_pvalue = cor.test(predictor, non_linear, method="pearson")$p.value,
        cor_non_linear2_min = cor.test(predictor, non_linear_2, method="pearson")$conf.int[1],
        cor_non_linear2 = cor.test(predictor, non_linear_2, method="pearson")$estimate,
        cor_non_linear2_max = cor.test(predictor, non_linear_2, method="pearson")$conf.int[2],
        cor_non_linear2_pvalue = cor.test(predictor, non_linear_2, method="pearson")$p.value,
        cor_non_relationship_min = cor.test(predictor, non_relationship, method="pearson")$conf.int[1],
        cor_non_relationship = cor.test(predictor, non_relationship, method="pearson")$estimate,
        cor_non_relationship_max = cor.test(predictor, non_relationship, method="pearson")$conf.int[2],
        cor_non_relationship_pvalue = cor.test(predictor, non_relationship, method="pearson")$p.value
    )
# Make a tibble 1x16 to 4x4
correlations_pearson <- matrix(unlist(correlations_pearson[,1:16]), 4, 4, byrow = TRUE) %>% 
    as_tibble() %>% 
    rename(
        "Lower Limit" = V1,
        "Estimate" = V2,
        "Upper Limit" = V3,
        "PValue" = V4
    )

# Add a column with the name of the correlation
correlations_pearson <- correlations_pearson %>% 
    mutate(
        n = c("Line", "Non linear", "Non linear 2", "Non relationship")
    )

# Using colum as row names
correlations_pearson <- correlations_pearson %>% 
    column_to_rownames("n")
correlations_pearson

cor.test(df$predictor, df$line_pattern)
