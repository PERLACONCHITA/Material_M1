# Reto 1. Distribuciones binomial, normal y t de Student

#### Distribuci�n binomial

# Consideremos un experimento binomial con n = 35 pruebas id�nticas e independientes, 
#en donde la probabilidad de �xito en cada prueba es p = 0.51. Encuentre lo siguiente: 

# 1. La probabilidad de observar exactamente 10 �xitos
dbinom(x = 10, size = 35, prob = 0.51)

# 2. La probabilidad de observar 10 o m�s exitos
pbinom(q = 10, size = 35, prob = 0.51)

# 3. El cuantil de orden 0.5
qbinom(p = 0.5, size = 35, prob = 0.51) 


# 4. Genere una muestra aleatoria de tama�o 1000 
#de esta distribuci�n, construya una tabla de frecuencias relativas con los resultados 
#y realice el gr�fico de barras de los resultados que muestre las frecuencias relativas.

set.seed(4)
muestra.a <- rbinom(n = 1000, size = 35, prob = 0.51)
muestra.a.frame <- as.data.frame(table(muestra.a)/length(muestra.a))

ggplot(muestra.a.frame, aes(x = muestra.a, y = Freq))+
  geom_bar(stat = "identity")

