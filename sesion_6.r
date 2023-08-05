# numerica vs categorica (categorica vs numerica)
# categorica vs numerica hay dos casos:
# 1. hay dos categorias (niveles) - t.test
# 2. hay mas de dos categorias (niveles)
# categorica vs categorica
# numerica vs numerica

# Carga la libreria para trabajar con los datos
library(tidyverse)

# Read cereal data
cereal <- read_csv("datasets/cereal.csv")
# This show the first 6 rows of the dataset
head(cereal)
# This print column names
colnames(cereal)

# summary of ceral manufacturer
cereal %>% group_by(mfr) %>% summarise(count = n())

# filter manufacturer only G y K
cereal_gk <- cereal %>% 
    filter(mfr == "G" | mfr == "K") %>%
    # from the result select only this columns
    select(mfr, calories)

head(cereal_gk)

# Quiero saber si hay relacion o no entre 
# el fabricante y las calorias

# H0: No hay relacion entre el fabricante y las calorias
# H1: Si hay relacion entre el fabricante y las calorias

k_histogram <- cereal_gk %>% 
    ggplot(aes(x = calories, group=mfr, color=mfr, fill=mfr)) +
    geom_histogram(alpha = 0.5)

k_histogram

k_calories <- cereal_gk %>% 
    filter(mfr == "K") %>%
    pull(calories)

g_calories <- cereal_gk %>% 
    filter(mfr == "G") %>%
    pull(calories)

# H0: No hay relacion entre el fabricante y las calorias
# H1: Si hay relacion entre el fabricante y las calorias

# Si el p-valor es mayor 0.05 no hay evidencia en contra H0
# Si el p-valor es menor o igual a 0.05 hay evidencia en contra H0

# Aqui es necesario separar los datos por grupos
t.test(k_calories, g_calories, conf.level = 0.30,
       var.equal = FALSE)

# forma alternativa usando los datos directamente
t.test(calories ~ mfr, data = cereal_gk, conf.level = 0.30,
       var.equal = FALSE)
