library(tidyverse)

AmesHousing <- read_csv("datasets/AmesHousing.csv")
glimpse(AmesHousing)

AmesHousing %>% group_by(`Kitchen Qual`) %>% summarise(count = n())
AmesHousing %>% group_by(`Pool QC`) %>% summarise(count = n())

# Como solo hay prueba para dos niveles
# Una opcion es dicotomizar los valores de `Kitchen Qual`
AmesHousing <- AmesHousing %>% 
  mutate(Kitchen_Qual_Bin = ifelse(`Kitchen Qual` %in% c("Ex", "Gd"), "Good", "Bad"))

AmesHousing %>% group_by(Kitchen_Qual_Bin) %>% summarise(count = n())

t.test(SalePrice ~ Kitchen_Qual_Bin, data = AmesHousing, conf.level = 0.30,
       var.equal = FALSE)


# Boxplot 
# Esto sirve para variables categoricas con mas de 2 niveles
AmesHousing %>% ggplot(aes(x = `Kitchen Qual`, y = SalePrice)) +
  geom_boxplot()

# Para el caso de Pool QC debo primero aplicar un filtro
Ames2 <- AmesHousing %>% filter(!is.na(`Pool QC`))
glimpse(Ames2)

Ames2 <- Ames2 %>% 
  mutate(Pool_Qual_Bin = ifelse(`Pool QC` %in% c("Ex", "Gd"), "Good", "Bad"))

Ames2 %>% group_by(Pool_Qual_Bin) %>% summarise(count = n())

t.test(SalePrice ~ Pool_Qual_Bin, data = Ames2, conf.level = 0.30,
       var.equal = FALSE)


# Algo que se puede hacer con la dicotomizacion es crear nuevas variables
AmesHousing <- AmesHousing %>% 
   mutate(is_there_pool = ifelse(is.na(`Pool QC`), "No", "Yes"))
glimpse(AmesHousing)

AmesHousing %>% group_by(is_there_pool) %>% summarise(count = n())

t.test(SalePrice ~ is_there_pool, data = AmesHousing, conf.level = 0.30,
       var.equal = FALSE)
