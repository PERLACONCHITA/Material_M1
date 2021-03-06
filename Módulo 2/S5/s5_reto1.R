# Reto 1. Regresi�n lineal simple

# Se cree que entre las variables x y y 
#del archivo csv adjunto, 
#podr�a haber una relaci�n m�s o menos lineal. 
#Para tener m�s evidencia sobre esto lleve a cabo 
#lo siguiente:

# 1. Realice el gr�fico de dispersi�n de los datos


datoslineal <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-con-R-Santander/master/Sesion-05/Reto-01/datoslineal.csv", header = TRUE)
View(datoslineal)

attach(datoslineal)

plot(x,y)

# 2. Ajuste un modelo de regresi�n lineal 
#simple a los datos, muestre un resumen 
#del modelo ajustado y trace la recta 
#de regresi�n estimada junto con el gr�fico de dispersi�n
modelo <- lm(lm(y ~ x))
abline(lsfit(x, y))
summary(modelo)



# 3. Obtenga algunas gr�ficas de 
#diagn�stico y diga si es razonable 
#suponer para los errores aleatoriedad,
#normalidad y varianza constante.

par(mfrow = c(2,2)) #:(
plot(modelo)

