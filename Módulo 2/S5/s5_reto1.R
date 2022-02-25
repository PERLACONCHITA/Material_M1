# Reto 1. Regresión lineal simple

# Se cree que entre las variables x y y 
#del archivo csv adjunto, 
#podría haber una relación más o menos lineal. 
#Para tener más evidencia sobre esto lleve a cabo 
#lo siguiente:

# 1. Realice el gráfico de dispersión de los datos


datoslineal <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-con-R-Santander/master/Sesion-05/Reto-01/datoslineal.csv", header = TRUE)
View(datoslineal)

attach(datoslineal)

plot(x,y)

# 2. Ajuste un modelo de regresión lineal 
#simple a los datos, muestre un resumen 
#del modelo ajustado y trace la recta 
#de regresión estimada junto con el gráfico de dispersión
modelo <- lm(lm(y ~ x))
abline(lsfit(x, y))
summary(modelo)



# 3. Obtenga algunas gráficas de 
#diagnóstico y diga si es razonable 
#suponer para los errores aleatoriedad,
#normalidad y varianza constante.

par(mfrow = c(2,2)) #:(
plot(modelo)

