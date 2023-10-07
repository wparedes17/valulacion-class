library(tidyverse)

df <- read_csv("datasets/diamonds - diamonds.csv")

# windows es window()
x11()
g <- df %>% ggplot(aes(x = price, y = carat)) +
  geom_point()


# This line is to save the plot
# ggsave(filename = "plots/overplotting.png", plot = g, width = 10, height = 10)
ggsave("plots/overplotting.png", g, width = 10, height = 10)

h <- g + geom_smooth(formula = y ~ x, method = "lm", se = FALSE)
h

cereal <- read_csv("datasets/cereal.csv")


# Categorica 2 niveles vs numerica
cereal %>% group_by(mfr) %>% summarise(
  n = n(),
)

cereal <- cereal %>% mutate(
    BigMFR = if_else(mfr == "K" | mfr == "G", "Big", "Small"))

cereal %>% group_by(BigMFR) %>% summarise(
  n = n(),
)

cereal %>% ggplot(aes(x = BigMFR, y = calories)) +
  geom_boxplot()

cereal %>% ggplot(aes(x = calories, fill = BigMFR)) +
  geom_histogram()

# Alter form
t.test(sugars ~ BigMFR, data = cereal)

sample_a <- cereal %>% filter(BigMFR == "Big") %>% pull(sugars)
sample_b <- cereal %>% filter(BigMFR == "Small") %>% pull(sugars)

t.test(sample_a, sample_b, var.equal = FALSE)

# Numerica vs numerica
cereal %>% ggplot(aes(x = carbo, y = calories)) +
  geom_point()

cor(cereal$carbo, cereal$calories)
cor.test(cereal$carbo, cereal$calories)
# Categorica vs categorica

tabla_contingencia <- cereal %>% group_by(mfr, type) %>% summarise(
  n = n(),
) %>% spread(type, n) %>% ungroup() %>% select(H,C)

tabla_contingencia

prop.test(as.matrix(tabla_contingencia))

as.matrix(tabla_contingencia)
