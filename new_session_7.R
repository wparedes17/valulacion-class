library(tidyverse)

consumo_gasolina <- read_csv("./datasets/consumo_gasolina.csv")
head(consumo_gasolina)

# boxplot between Tipo and Consumo
consumo_gasolina %>%
    ggplot(aes(x = Tipo, y = Consumo)) +
    geom_boxplot() +
    labs(title = "Boxplot between Tipo and Consumo", x = "Tipo", y = "Consumo") +
    theme_minimal()

# violin plot between Tipo and Consumo
consumo_gasolina %>%
    ggplot(aes(x = Tipo, y = Consumo)) +
    geom_violin() +
    labs(title = "Violin plot between Tipo and Consumo", x = "Tipo", y = "Consumo") +
    theme_minimal()


fit_aov <- aov(Consumo ~ Tipo, data = consumo_gasolina)
summary(fit_aov)

residuos <- tibble(
    id = 1:length(residuals(fit_aov)),
    residuos = residuals(fit_aov)
    )
# We must check the normality of the residuals
 residuos %>% ggplot(aes(x = residuos)) +
      geom_histogram(aes(y = after_stat(count/sum(count))), binwidth = 1, fill = "blue", color = "black") +
      labs(title = "Histogram of Residuals", x = "Residuals", y = "Density") +
      theme_minimal()
#
residuos %>% ggplot(aes(y = residuos, x = id)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "red") +
    labs(title = "Randomness plot") +
    theme_minimal()

#
residuos %>% ggplot(aes(sample = residuos)) +
    stat_qq() +
    geom_abline() +
    labs(title = "QQ Plot of Residuals") +
    theme_minimal()
