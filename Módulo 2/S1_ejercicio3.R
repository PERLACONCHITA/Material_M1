# obtenemos la ruta del directorio del trabajo
getwd()

# fijando el directorio de trabajo
setwd() #aquí fijo mi directorio de trabajo

# read.csv es util para leer el fichero debe estar en el directorio de trabajo
#read.csv("bestsellers with categories.csv") # El archivo csv debe estar en el directorio de trabajo

#también se le puede poner el nombre
#cuadro<- read.csv("bestsellers with categories.csv") 

# se puede asignar a un objeto el fichero leido anteriormente

amazon.books <- read.csv("bestsellers with categories.csv")
tail(amazon.books)
str(amazon.books)


#también se puede leer el fichero directamente de una URL

data.url <- read.csv("https://www.football-data.co.uk/mmz4281/2021/SP1.csv")
tail(data.url)
str(data.url)

View(data.url)  

# calcular la dimensión de la base de datos
dim(data.url)

#el tipo de objeto se puede saber utilizando class(
class(data.url)

#RETO1----------------
#Importar el archivo
Reto1 <- read.csv("https://raw.githubusercontent.com/ecoronadoj/Sesion_1/main/Data/netflix_titles.csv")
View(Reto1)
dim(Reto1)

#Peliculas que salieron después del 2015
Reto1$release_year
net.2015 <- Reto1[Reto1$release_year >2015,]
net.2015
View(net.2015)

#Guardar el archivo en una carpeta de la compu
write.csv(net.2015,file = "res.netflix.csv")


