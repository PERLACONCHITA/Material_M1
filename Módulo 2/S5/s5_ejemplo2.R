# Ejemplo 2. Regresi�n Lineal M�ltiple

# Predecir el precio de cena (platillo). 
# Datos de encuestas de clientes de 168 restaurantes Italianos
# en el �rea deseada est�n disponibles.

# Y: Price (Precio): el precio (en USD) de la cena
# X1: Food: Valuaci�n del cliente de la comida (sacado de 30)
# X2: D�cor: Valuaci�n del cliente de la decoraci�n (sacado de 30)
# X3: Service: Valuaci�n del cliente del servicio (sacado de 30)
# X4: East: variable dummy: 1 (0) si el restaurante est� al este (oeste) de la quinta avenida

# Primero debemos establecer nuestro directorio de trabajo y el archivo
# de datos (nyc.csv) que importaremos a R deber� de estar en este directorio
library(dplyr)


nyc <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-con-R-2021/main/Sesion-05/Ejemplo-02/nyc.csv", header = TRUE)

# Observamos algunas filas y la dimensi�n del data frame

str(nyc)
head(nyc, 2); 
tail(nyc, 2); 
dim(nyc)
attach(nyc)

nyc$East

# Llevamos a cabo el ajuste de un modelo
# Y = beta0 + beta1*Food + beta2*Decor + beta3*Service + beta4*East + e

m1 <- lm(Price ~ Food + Decor + Service + East)

# Obtenemos un resumen

summary(m1)

# Ajustamos nuevamente un modelo pero ahora sin considerar la variable Service
# ya que en el resultado anterior se observ� que su coeficiente de regresi�n
# no fue estad�sticamente significativo

# Y = beta0 + beta1*Food + beta2*Decor + beta4*East + e (Reducido)

m2 <- lm(Price ~ Food + Decor + East)

# Obtenemos un resumen del modelo ajustado

summary(m2)

# Una forma alternativa de obtener m2 es usar el comando update

m2 <- update(m1, ~.-Service)
summary(m2)

######

# An�lisis de covarianza

# Para investigar si el efecto de los predictores depende de la variable dummy 
# East consideraremos el siguiente modelo el cual es una extensi�n a m�s de una 
# variable predictora del modelo de rectas de regresi�n no relacionadas 
# Y = beta0 + beta1*Food + beta2*Decor +  beta3*Service + beta4*East 
#           + beta5*Food*East + beta6*Decor*East + beta7*Service*East + e (Completo)

mfull <- lm(Price ~ Food + Decor + Service + East + 
              Food:East + Decor:East + Service:East)

# Note como ninguno de los coeficientes de regresi�n para los
# t�rminos de interacci�n son estad�sticamente significativos

summary(mfull)

# Ahora compararemos el modelo completo guardado en mfull contra el modelo
# reducido guardado en m2. Es decir, llevaremos a cabo una prueba de hip�tesis
# general de

# H0: beta3 = beta5 = beta6 = beta7 = 0
# es decir Y = beta0 + beta1*Food + beta2*Decor + beta4*East + e (Reducido)
# contra
# H1: H0 no es verdad
# es decir, 
# Y = beta0 + beta1*Food + beta2*Decor +  beta3*Service + beta4*East 
#           + beta5*Food*East + beta6*Decor*East + beta7*Service*East + e (Completo)

# La prueba de si el efecto de los predictores depende de la variable dummy
# East puede lograrse usando la siguiente prueba-F parcial.

anova(m2,mfull)

# Dado que el p-value es aproximadamente 0.36, fallamos en rechazar la hip�tesis
# nula y adopatamos el modelo reducido
# Y = beta0 + beta1*Food + beta2*Decor + beta4*East + e (Reducido)

######

# Diagn�sticos

# En regresi�n m�ltiple, las gr�ficas de residuales o de residuales
# estandarizados proporcionan informaci�n directa sobre la forma
# en la cual el modelo est� mal especificado cuando se cumplen
# las siguientes dos condiciones:

# E(Y | X = x) = g(beta0 + beta1*x1 + ... + betap*xp) y
# E(Xi | Xj) aprox alpha0 + alpha1*Xj

# Cuando estas condiciones se cumplen, la gr�fica de Y contra
# los valores ajustados, proporciona informaci�n directa acerca de g.
# En regresi�n lineal m�ltiple g es la funci�n identidad. En
# este caso la gr�fica de Y contra los valores ajustados
# debe producir puntos dispersos alrededor de una recta.
# Si las condiciones no se cumplen, entonces un patr�n en la
# gr�fica de los residuales indica que un modelo incorrecto
# ha sido ajustado, pero el patr�n mismo no proporciona 
# informaci�n directa sobre como el modelo est� mal espec�ficado.

# Ahora tratemos de verificar si el modelo ajustado es un modelo v�lido.

# Acontinuaci�n mostramos una matriz de gr�ficos de dispersi�n de los
# tres predictores continuos. Los predictores parecen estar linealmente
# relacionados al menos aproximadamente

pairs(~ Food + Decor + Service, data = nyc, gap = 0.4, cex.labels = 1.5)

# Acontinuaci�n veremos gr�ficas de residuales estandarizados contra cada
# predictor. La naturaleza aleatoria de estas gr�ficas es un indicativo de
# que el modelo ajustado es un modelo v�lido para los datos.


m1 <- lm(Price ~ Food + Decor + Service + East)
summary(m1)
StanRes1 <- rstandard(m1)
par(mfrow = c(2, 2))
plot(Food, StanRes1, ylab = "Residuales Estandarizados")
plot(Decor, StanRes1, ylab = "Residuales Estandarizados")
plot(Service, StanRes1, ylab = "Residuales Estandarizados")

plot(East, StanRes1, ylab = "Residuales Estandarizados")
dev.off()

# Finalmente mostramos una gr�fica de Y, el precio contra los valores
# ajustados 

plot(m1$fitted.values, 
     Price, xlab = "Valores ajustados", 
     ylab = "Price")
abline(lsfit(m1$fitted.values, Price))

# Inspirado en:

# [S.J. Sheather, A Modern Approach to Regression with R, DOI: 10.1007/978-0-387-09608-7_2, � Springer Science + Business Media LLC 2009](https://gattonweb.uky.edu/sheather/book/index.php)
