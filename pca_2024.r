library(tidyverse)

tallas <- read_csv("datasets/tallas.csv")

# Only to check the data (we can repeat this whenever we want)
head(tallas)
glimpse(tallas)

# We remove tamamer column because it is not a numeric variable
# and also we remove idloc column because is only an identifier
talla_num <- tallas %>% select(-tamamer, -idloc, -idmercado, -promo, -ropamujer)

# We check the data again
summary(talla_num)

library(GGally)
ggpairs(talla_num)


library(corrplot)
m <- cor(talla_num)
corrplot(m, method = "ellipse", type = "upper", order = "AOE")


cnames <- colnames(talla_num)
png("pca_2024.png")
par(mfrow=c(3,3))
for (i in 1:7) {
    qqnorm(as.matrix(talla_num)[,i], main=cnames[i])
}
dev.off()

# Box cox transformation
# A particular case of Box-Cox transformation is the logarithm transformation, which is equivalent to Box-Cox transformation with lambda=0.
# Logaitm transformation is used when the data is right-skewed

# In this case, for example nomina is right-skewed
nomina <- talla_num$nomina
# histogram + density plot of nomina
hist(nomina, freq = FALSE, col = "lightblue", breaks = 20)
lines(density(nomina), col = "red", lwd = 2)

# apply log transformation
nomina_log <- log(nomina)
# histogram + density plot of nomina_log
hist(nomina_log, freq = FALSE, col = "lightblue", breaks = 20)
lines(density(nomina_log), col = "red", lwd = 2)

# Box cox transformation is the best way to find the best lambda value
# for other cases. 
library(MASS)

# Check the plot, and determine if the interval includes 0
# If 0 is included, then log transformation is the best option
l_model <- boxcox(lm(nomina ~ 1))

# Other case, best lambda value is the one that maximizes the y value
lambda <- l_model$x[which.max(l_model$y)]

# Now, we apply the transformation
# new_x_exact <- (x ^ lambda - 1) / lambda
nomina_boxcox <- (nomina^lambda - 1) / lambda

# histogram + density plot of nomina_boxcox
hist(nomina_boxcox, freq = FALSE, col = "lightblue", breaks = 20)
lines(density(nomina_boxcox), col = "red", lwd = 2)

