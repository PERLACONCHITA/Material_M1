# Media
x = c(4000, 9000, 9000, 10000); mean(x)

# mediana
median(x)

install.packages("DescTools")
library(DescTools)
Mode(x)

# quantiles
x <- c(29, 13, 62, 4, 63, 96, 1, 90, 50, 46)

mean(x)

quantile(x, 0.25) #cuantil del 25%
quantile(x, c(0.25,0.50, 0.75))
quantile(x, seq(0.1, 0.9, by = 0.1)) #deciles

seq(0.1, 0.9, by =0.1)

# Medidas de dispersión
# Calcular el rango intercuartilico 
IQR(x)

# opción 2
quantile(x, probs = 0.75) - quantile(x, probs = 0.25)

#la varianza y desviacion estandar
var(x) #cuanto se desvian de la media mis datos
sd(x)


