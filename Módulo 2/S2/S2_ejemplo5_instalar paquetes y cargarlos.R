# apply regresa un vector, arreglo o lista de valores
# obtenidos al aplicar una funci�n a los margenes
#de un arreglo o matriz
library(dplyr)

X <- matrix(1:49, ncol = 7)
X
apply(X, 1, mean) # c�lculo de la media para las filas
apply(X, 2, median) # c�lculo de la mediana para las columnas


# lapply(X,FUN,...)
# X es un vector o una lista
# FUN es la funci�n aplicada a cada elemento de x
#... son los argumentos opcionales
u1011 <- "https://www.football-data.co.uk/mmz4281/1011/SP1.csv"
u1112 <- "https://www.football-data.co.uk/mmz4281/1112/SP1.csv"
u1213 <- "https://www.football-data.co.uk/mmz4281/1213/SP1.csv"
u1314 <- "https://www.football-data.co.uk/mmz4281/1314/SP1.csv"

download.file(url = u1011, destfile = "SP1-1011.csv", mode = "wb")
download.file(url = u1112, destfile = "SP1-1112.csv", mode = "wb")
download.file(url = u1213, destfile = "SP1-1213.csv", mode = "wb")
download.file(url = u1314, destfile = "SP1-1314.csv", mode = "wb")

#vemos lo que hemos descargado o lo que esta en la carpeta
dir()
getwd()
setwd("c:/Users/52333/Dropbox/Mi PC (LAPTOP-569HTKOE)/Documents/Prueba Git/TECP0013ADDAOL2112/M�dulo 2/csv")
dir()

#con lapply leemos con una solo instruccion
#los archivos descargados
lista <- lapply(dir(), read.csv) # Guardamos los archivos en lista
View(lista)
head(lista)
str(lista)

#los elementos de lista son los archivos cvs leidos
#y que estan como data frames
lista <- lapply(lista, select, Date:FTR) # seleccionamos solo algunas columnas de cada data frame
View(lista)

head(lista[[1]]); 
head(lista[[2]]); 
head(lista[[3]]); 
head(lista[[4]])

data <- do.call(rbind, lista)
data$Date
t1 <- mutate(data, Date = as.Date(Date, "%d/%m/%Y"))
t1


?do.call
?rbind
head(data)
dim(data)
View(data)

head(t1)
