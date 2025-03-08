library(tidyverse)

example_table <- tibble(
    activo = c(rep("Desipramina", 24), rep("Litio", 24), rep("Placebo", 24)),
    recaidas = c(rep("Si", 10), rep("No", 14), rep("Si", 18), rep("No", 6), rep("Si", 20), rep("No", 4))
)

example_table %>%
    count(activo, recaidas) %>%
    ggplot(aes(x = activo, y = n, fill = recaidas)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Example Table", x = "Treatment", y = "Count") +
    theme_minimal()

ct <- table(example_table$activo, example_table$recaidas)
chisq.test(ct)
