#Histogramas
library(dplyr) # Para usar el operador %>%
library(ggplot2)
data <- read.csv("c:/Users/52333/Dropbox/Mi PC (LAPTOP-569HTKOE)/Documents/Prueba Git/TECP0013ADDAOL2112/Módulo 2/csv/boxp.csv")

data = read.csv(file.choose()) #elijo donde lo guarde en documentos

head(data)
names(data)

#modificando una columna para ocultar los datos reales en caso de compartirlos
data2 <- mutate(data, Mediciones = Mediciones*1.23) # se multilica por un factor la columna
head(data2)

data3 <- mutate(data2, Mediciones = Mediciones/1.23) # se regresa a su valor original
head(data3)

# Utilizando la función hist

hist(data$Mediciones, breaks = seq(0,300, 20), 
     main = "Histograma de Mediciones",
     xlab = "Mediciones",
     ylab = "Frecuencia")

lines(x=100,y=100)

lines(data$Mediciones, breaks = seq(0,300, 20), 
     main = "Histograma de Mediciones",
     xlab = "Mediciones",
     ylab = "Frecuencia")

# Ahora utilizando ggplot para apreciar los resultados de las dos funciones

# Evitar el Warning de filas con NA´s

data <- na.omit(data) 
summary(data)


 #%>% ya no tengo que decir como se llama la base

data %>%
  ggplot() + 
  aes(Mediciones) +
  geom_histogram(binwidth = 10) #si no le pongo binwidth solito elige los grupos que debe utilizar

# Agregando algunas etiquetas y tema, intenta modificar algunas de las opciones para que aprecies los resultados

data %>%
  ggplot() + 
  aes(Mediciones) +
  geom_histogram(binwidth = 10, col="black", fill = "blue") + 
  ggtitle("Histograma de Mediciones") +
  ylab("Frecuencia") +
  xlab("Mediciones") + 
  theme_light()

# Tanto hist(), como ggplot() + aes() 
#+ geom_histogram() son útiles para generar 
#+ #los histogramas, tu decide cual te funciona mejor.

