#Considera el siguiente vector
set.seed(134)
x <- round(rnorm(1000, 175, 6), 1)
library(DescTools)

#Calcule la media, mediana y moda de los valores x

mean(x)   #media
median(x) #mediana
Mode(x)   #Mode

#Obtenga los deciles de los numeros en x
quantile(x,seq(0.1,0.9, by=0.1))

#Encuentre la rango intercuartílico, la desviación estándar y varianza muestral de las mediciones en x
IQR(x)  #rango intercuantilico
var(x)  #varianza
sd(x)   #desviacion estandar
