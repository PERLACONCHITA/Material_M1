# Distribuciones binomial, normal y t de Student

library(ggplot2)
install.packages("reshape2")
library(reshape2)

# Distribuci�n binomial. La usamos para experimentar

# En `R` para calcular valores de las funciones 
#de probabilidad, distribuci�n 
# o cuantiles de la distribuci�n binomial 
#(discreta), usamos las funciones dbinom,
# pbinom y  qbinom respectivamente. 
#Para generar muestras aleatorias de esta
# distribuci�n utilizamos la funci�n rbinom.

# Consideremos un experimento binomial con n = 30 pruebas 
# id�nticas e independientes, en donde la probabilidad de �xito en cada prueba
# es p = 0.2 (par�metros n = 30 y p = 0.2)

#### Funci�n de probabilidad

# Para obtener P(X = 20), es decir, la probabilidad de observar
# 20 �xitos exactamente, en `R` ejecutamos

dbinom(x = 20, size = 30, prob = 0.2) #de los 30 intentos cual es la probabilidad de que 20 sean �guila

#### Funci�n de distribuci�n

# Para obtener P(X <= 20), es decir, la probabilidad de observar
# a lo m�s 20 �xitos, en `R` corremos

pbinom(q = 20, size = 30, prob = 0.2)
# valor acumulado a la izquierda


# Para encontrar el valor m�s peque�o b tal que P(X <= b) >= 0.35,
# es decir, el cuantil de orden 0.35, usamos

#### Cuantiles

qbinom(p = 0.35, size = 30, prob = 0.2) # b = 5

pbinom(q = 4, size = 30, prob = 0.2) # P(X <= 4) = 0.2552 < 0.35

pbinom(q = 5, size = 30, prob = 0.2) # P(X <= 5) = 0.4275 >= 0.35
pbinom(q = 6, size = 30, prob = 0.2) # P(X <= 6) = 0.6070 >= 0.35

#### Muestras aleatorias

# Para obtener una muestra aleatoria 
#de tama�o n = 1000, de la
# distribuci�n binomial con par�metros como especificamos,
# hacemos

set.seed(4857) # Establecemos una semilla, para poder reproducir la muestra en el futuro
#el numero es el que asignamos a esa semilla
muestra <- rbinom(n = 1000, size = 30, prob = 0.2) #numero de observaciones, numero de ensayos
length(muestra); muestra[1:3]


# Podemos observar las frecuencias absolutas de los distintos valores
# obtenidos

as.data.frame(table(muestra))

# Tambi�n podemos observar las frecuencias relativas

(df1 <- as.data.frame(table(muestra)/length(muestra)))
str(df1)

#para transformarlo en numero
valg <- as.character(df1$muestra) # distintos valores generados por rbinom
(valg <- as.numeric(valg)) # Convertimos a n�meros

# Las frecuencias relativas son muy parecidas a las siguientes probabilidades

(v1 <- round(sapply(valg, dbinom, size = 30, p = 0.2), 3))

# Combinamos df1 y v1 en un �nico data frame

(df2 <- cbind(df1, v1))
(names(df2) <- c("Exitos", "FR", "Prob"))

(df2 <- melt(df2)) # funci�n del paquete reshape2

# Las frecuencias relativas son muy parecidas a las probabilidades.

ggplot(df2, aes(x = Exitos, y = value, fill = variable)) + 
  geom_bar (stat="identity", position = "dodge") # Funciones del paquete ggplot2

# Distribuci�n normal

# En `R` para calcular valores de las funciones de densidad, distribuci�n 
# o cuantiles de la distribuci�n normal (continua), usamos las funciones dnorm,
# pnorm y  qnorm respectivamente. Para generar muestras aleatorias de esta
# distribuci�n utilizamos la funci�n rnorm.

# Consideremos una variable aleatoria (v.a.) X que se distribuye como normal
# con media 175 y desviaci�n est�ndar 6 (par�metros mu = 175 y sigma = 6)

#### Funci�n de densidad

x <- seq(-4, 4, 0.01)*6 + 175 # Algunos posibles valores que puede tomar la v.a. X (m�nimo: mu-4sigma, m�ximo: mu+4sigma)
y <- dnorm(x, mean = 175, sd = 6) # Valores correspondientes de la funci�n de densidad de probabilidad

plot(x, y, type = "l", xlab = "", ylab = "")
  title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 175, " y ", sigma == 6)))
abline(v = 175, lwd = 2, lty = 2) # La media es 175 
#type = "l", significa que usaremos una linea, porque son continuos


plot(x, y, type = "h", xlab = "", ylab = "")
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 175, " y ", sigma == 6)))
abline(v = 175, lwd = 2, lty = 2) # La media es 175 
#la h es para histograma




#### Funci�n de distribuci�n

# Para obtener P(X <= 180), es decir, la probabilidad de que X tome un valor
# menor o igual a 180, ejecutamos

pnorm(q = 180, mean = 175, sd = 6)

(par(mfrow = c(2, 2)))


#mfrow combina gr�ficos
# Datos
x <- rexp(50)

# Una fila, dos columnas
par(mfrow = c(1, 2))
par

# Los siguientes gr�ficos se combinar�n
hist(x, main = "Gr�fico izquierda")  # Izquierda
boxplot(x, main = "Gr�fico derecha") # Derecha

# Volvemos al estado original
par(mfrow = c(1, 1)) 





# Observemos la regi�n que corresponde a esta probabilidad en la siguiente gr�fica en color rojo

plot(x, y, type = "l", xlab = "", ylab = "")
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 175, " y ", sigma == 6)))

#selecciona el �rea que indica
polygon(c(min(x), x[x<=180], 180), c(0, y[x<=180], 0), col="red")

# Para obtener P(X <= 165), es decir, la probabilidad de que X tome un valor
# menor o igual a 165, ejecutamos

pnorm(q = 165, mean = 175, sd = 6)

# Observemos la regi�n que corresponde a esta probabilidad en la siguiente gr�fica en color amarillo

plot(x, y, type = "l", xlab = "", ylab = "")
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 175, " y ", sigma == 6)))

polygon(c(min(x), x[x<=165], 165), c(0, y[x<=165], 0), col="yellow")

# Para obtener P(165 <= X <= 180), es decir, la probabilidad de que X tome un valor
# mayor o igual a 165 y menor o igual a 180, debemos correr

pnorm(q = 180, mean = 175, sd = 6) - pnorm(q = 165, mean = 175, sd = 6)

# Observemos la regi�n que corresponde a esta probabilidad en la siguiente gr�fica en color verde

plot(x, y, type = "l", xlab="", ylab="")
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 175, " y ", sigma == 6)))

polygon(c(165, x[x>=165 & x<=180], 180), c(0, y[x>=165 & x<=180], 0), col="green")

# Para obtener P(X >= 182), es decir, la probabilidad de que X tome un valor
# mayor o igual a 182, una alternativa es 


pnorm(q = 182, mean = 175, sd = 6, lower.tail = FALSE)

# Observemos la regi�n que corresponde a esta probabilidad en la siguiente gr�fica en color azul

plot(x, y, type = "l", xlab="", ylab="")
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 175, " y ", sigma == 6)))

polygon(c(182, x[x>=182], max(x)), c(0, y[x>=182], 0), col="blue")

dev.off() # Para mostrar solo una gr�fica

#### Cuantiles

# Para encontrar el n�mero b, tal que P(X <= b) = 0.75, es decir,
# el cuantil de orden 0.75, ejecutamos

(b <- qnorm(p = 0.75, mean = 175, sd = 6)) 

# Comprobamos

pnorm(b, 175, 6)

# El cuantil se encuentra en el eje de medici�n (eje horizontal)


plot(x, y, type = "l", xlab="", ylab="")
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 175, " y ", sigma == 6)))

axis(side = 1, at = b, font = 2, padj = 1, lwd = 2)

#### Muestras aleatorias

# Para generar una muestra aleatoria de tama�o n = 1000 de la v.a. X
# corremos la siguiente instrucci�n

set.seed(7563) # Para poder reproducir la muestra en el futuro
muestra <- rnorm(n = 1000, mean = 175, sd = 6)
length(muestra); mdf <- as.data.frame(muestra)
tail(mdf)

# Observamos que el histograma de la muestra generada tiene forma de campana
# Similar a la densidad de una normal

ggplot(mdf, aes(muestra)) + 
  geom_histogram(colour = 'red', 
                 fill = 'blue',
                 alpha = 0.3, # Intensidad del color fill
                 binwidth = 3) + 
  geom_density(aes(y = 3*..count..))+
  geom_vline(xintercept = mean(mdf$muestra), linetype="dashed", color = "black") + 
  ggtitle('Histograma para la muestra normal') + 
  labs(x = 'Valores obtenidos', y = 'Frecuencia')+
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))  

#### Regla emp�rica

mean <- 175; sd <- 6
x <- seq(mean-4*sd, mean+4*sd, 0.01)
y <- dnorm(x, mean, sd)
plot(x, y, type = "l", xlab="valores", ylab = "", xaxt = "n", yaxt = "n")
title(main = "Densidad de Probabilidad Normal", sub = expression(paste("Regla Emp�rica con ", mu == 175, " y ", sigma == 6)))
abline(v=mean, lty = 2, lwd = 2)
for(k in c(-3, -2, -1, 1, 2, 3)) abline(v = mean+k*sd, lty = 2, col = abs(k))
ps <- c(mean - 3*sd, mean - 2*sd, mean - sd, mean, mean + sd, mean + 2*sd, mean + 3*sd)
axis(side = 1, at = ps)
x0 <- NULL
for(i in 1:length(ps)-1) x0 <- c(x0, (ps[i]+ps[i+1])/2)
y0 <- dnorm(x0, mean, sd)*1/3
text(x = x0, y = y0, labels = c("2.35%", "13.5%", "34%", "34%", "13.5%", "2.35%"))
x1 <- (x[1]+ps[1])/2; y1 <- dnorm(mean, mean, sd)*1/2
xf <- (x[length(x)]+ps[length(ps)])/2; yf <- dnorm(mean, mean, sd)*1/2
text(x = c(x1, xf), y = c(y1, yf), labels = c("0.15%", "0.15%"))
segments(x0 = x1, y0 = 0, x1 = x1, y1 = y1,               # Draw one line as in Example 1
         col = "cornflowerblue",                               # Color of line
         lwd = 5,                                              # Thickness of line
         lty = "dotted")     
segments(x0 = xf, y0 = 0, x1 = xf, y1 = yf,               
         col = "cornflowerblue",                               
         lwd = 5,                                              
         lty = "dotted")     

# Distribuci�n t de Student

# En `R` para calcular valores de las funciones de densidad, distribuci�n 
# o cuantiles de la distribuci�n t de Student (continua), usamos las funciones dt,
# pt y  qt respectivamente. Para generar muestras aleatorias de esta
# distribuci�n utilizamos la funci�n rt.

# Consideremos una variable aleatoria (v.a.) T que se distribuye como t
# de Student con 7 grados de libertad (gl) (par�metro gl = 7)

#### Funci�n de densidad

x <- seq(-4, 4, 0.01) # Algunos valores que puede tomar la v.a. T con 7 gl
y <- dt(x, df = 7) # Valores correspondientes de la densidad t de Student con 7 gl
plot(x, y, type = "l", main = "Densidad t de Student, gl = 7", xlab="", ylab="")
abline(v = 0, lwd=2, lty=2)

#### Funci�n de distribuci�n

# Para encontrar P(T <= 1.5), ejecutamos la siguiente instrucci�n

pt(q = 1.5, df = 7)

# Observemos la regi�n que corresponde a esta probabilidad en la siguiente gr�fica

plot(x, y, type = "l", 
     main = "Densidad t de Student, 
     gl = 7", xlab="", ylab="")
polygon(c(min(x), x[x<=1.5], 1.5), 
        c(0, y[x<=1.5], 0), col="purple")

# Para encontrar P(T >= 2), ejecutamos

pt(q = 2, df = 7, lower.tail = FALSE)
pt

# Observemos la regi�n que corresponde 
#a esta probabilidad en la siguiente gr�fica

plot(x, y, type = "l", main = "Densidad t de Student, gl = 7", xlab="", ylab="")

polygon(c(2, x[x>=2], max(x)), c(0, y[x>=2], 0), col="orange")

#### Cuantiles

# Para encontrar el n�mero d tal que P(T <= d) = 0.025, es decir, 
# el cuantil de orden 0.025, corremos la siguiente instrucci�n

(d <- qt(p = 0.025, df = 7))

# comprobamos

pt(q = d, df = 7) #q es el cuantil

# Mostramos el cuantil contrado en el eje de medici�n (eje horizonta)

plot(x, y, type = "l", main = "Densidad t de Student, gl = 7", xlab="", ylab="")
axis(side = 1, at = d, font = 2, padj = 1, lwd = 2)

#### Muestras aleatorias

# Para generar una muestra aleatoria
#de tama�o n = 1000 de la v.a. T
# corremos la siguiente instrucci�n

set.seed(777) # Para poder reproducir la muestra en el futuro
muestra <- rt(n = 1000, df = 7)
length(muestra); mdf <- as.data.frame(muestra)
tail(mdf)

# Observamos que el histograma de la muestra generada tiene forma de campana
# similar a la densidad t de Student

ggplot(mdf, aes(muestra)) + 
  geom_histogram(colour = 'green', 
                 fill = 'orange',
                 alpha = 0.7, # Intensidad del color fill
                 binwidth = 0.5) + 
  geom_density(aes(y = 0.5*..count..))+
  geom_vline(xintercept = mean(mdf$muestra), linetype="dashed", color = "black") + 
  ggtitle('Histograma para la muestra t de Student') + 
  labs(x = 'Valores obtenidos', y = 'Frecuencia')+
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))  
