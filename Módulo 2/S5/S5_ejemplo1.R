# Ejemplo 1. Regresi�n Lineal Simple

# Primero hay que establecer el directorio de trabajo y este deber� contener 
# el archivo de datos production.txt

# Leemos nuestros datos con la funci�n read.table

library(dplyr)
library(tidyverse)

production <- read.table("https://raw.githubusercontent.com/beduExpert/Programacion-con-R-Santander/master/Sesion-05/Ejemplo-01/production.txt", header = TRUE)
View(production)

production <- read.table(file.choose(), header = TRUE)

# Los datos que importamos a R se 
#encuentran como data frame con nombre 
# production. Aplicamos la funci�n 
#attach al data frame production para
# poder manipular las columnas mediante 
#sus nombres

rm(list = ls()) # Para eliminar objetos creados previamente

attach(production)
str(prod)
View(production)


# Hacemos el gr�fico de dispersi�n

plot(RunSize, RunTime, 
     xlab = "Tama�o de ejecuci�n", 
     ylab = "Tiempo de ejecuci�n", 
     pch = 16)

# Ajustamos un modelo de regresi�n lineal simple con la funci�n lm, en donde
# la variable de respuesta es RunTime y la variable predictora es RunSize. 
# Guardamos nuestro modelo ajustado con el nombre de m1

m1 <- lm(RunTime~RunSize)

# Obtenemos un resumen de nuestro modelo ajustado mediante la funci�n `summary`

summary(m1)
View(m1$residuals)

# Graficamos nuestros datos nuevamente, pero ahora con la recta de regresi�n
# ajustada

plot(RunSize, RunTime, 
     xlab = "Tama�o de ejecuci�n", 
     ylab = "Tiempo de ejecuci�n", 
     pch = 16)
abline(lsfit(RunSize, RunTime)) # Trazamos la recta de regresi�n estimada
mtext(expression(paste('Modelo de regresi�n lineal simple:',
                       ' ',
                       y[i] == beta[0] + 
                         beta[1]*x[i] + 
                         e[i])),
      side = 3, adj=1, font = 2)

# Recta de regresi�n poblacional

text(x = 200, y = 240, expression(paste('Recta de regresi�n:',
                                        ' ',
                                        y[i] == beta[0] + beta[1]*x[i])),
     adj = 1, font = 2)


# Recta de regresi�n estimada

text(x = 350, y = 180, expression(paste('Recta estimada:',
                                        ' ',
                                        hat(y)[i] == hat(beta)[0] + hat(beta)[1]*x[i])),
     adj = 1, font = 2)

# Recta de regresi�n estimada

text(x = 350, y = 160, expression(paste('Recta estimada:',
                                        ' ',
                                        hat(y)[i] == 149.74770 + 0.25924*x[i])),
     adj = 1, font = 2)

# Residuales

points(189, 215, pch=16, col = "red") # Punto muestral
149.74770 + 0.25924 * 189 # Valor y sobre la recta estimada
lines(c(189, 189), c(198.7441, 215), col = "red")

points(173, 166, pch=16, col = "red") # Punto muestral
149.74770 + 0.25924 * 173 # Valor y sobre la recta estimada
lines(c(173, 173), c(166, 194.5962), col = "red")

# Acontinuaci�n encontramos el cuantil de orden 0.975 de la distribuci�n
# t de Student con 18 (n - 2) grados de libertad. En total tenemos n = 20 
# observaciones en nuestro conjunto de datos. Estamos encontrando el valor 
# que satisface P(T > tval) = 0.025

tval <- qt(1-0.05/2, 18)
tval

# Comprobamos

pt(tval, df = 18)

# Encontramos intervalos de confianza del 95% para el intercepto y la pendiente
# del modelo de regresi�n lineal simple

round(confint(m1, level = 0.95), 3)

# Ahora encontramos intervalos de confianza del 95% para la recta de regresi�n
# poblacional en algunos valores de X (RunSize)

RunSize0 <- c(50,100,150,200,250,300,350) # Algunos posibles valores de RunSize

(conf <- predict(m1, newdata = 
                   data.frame(RunSize = RunSize0), 
                 interval = "confidence", level = 0.95))

# Podemos visualizar gr�ficamente estos intervalos de confianza

lines(RunSize0, conf[, 2], lty = 2, lwd = 2, col = "green") # l�mites inferiores
lines(RunSize0, conf[, 3], lty = 2, lwd = 2, col = "green") # l�mites superiores

# Tambi�n podemos encontrar intervalos de predicci�n del 95% para el valor
# real de la variable de respuesta Y (RunTime) en algunos valores de X (RunSize)

(pred <- predict(m1, newdata = 
                   data.frame(RunSize = RunSize0), 
                 interval = "prediction", level = 0.95))

# Podemos visualizar gr�ficamente estos intervalos de predicci�n

lines(RunSize0, pred[, 2], lty = 2, lwd = 2, col = "blue") # l�mites inferiores
lines(RunSize0, pred[, 3], lty = 2, lwd = 2, col = "blue") # l�mites superiores


# Note como los intervalos de confianza est�n contenidos dentro de los
# intervalos de predicci�n correspondientes

# Tambi�n es posible llevar a cabo un an�lisis de varianza para decidir si 
# existe asociaci�n lineal entre RunSize y RunTime

anova(m1)

# Gr�fico de diagn�stico de R

# Cuando usamos un modelo de regresi�n, hacemos una serie de suposiciones. 
# Entonces debemos hacer diagn�sticos de regresi�n para verificar las
# supocisiones.

par(mfrow = c(2, 2))
plot(m1)
dev.off()

# Inspirado en:

# [S.J. Sheather, A Modern Approach to Regression with R, DOI: 10.1007/978-0-387-09608-7_2, � Springer Science + Business Media LLC 2009](https://gattonweb.uky.edu/sheather/book/index.php)



