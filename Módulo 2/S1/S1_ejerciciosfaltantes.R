saludo <- function(nombre){ mensaje <- paste("Hola", nombre) return(mensaje)}
?table

paste("Hola", nombre);

saludo <-function(nombre){mensaje<-paste("hola",nombre)
return(mensaje)
}
saludo
nombre

saludo("adios")

#listas
(milista <- list(nombre = "Pepe", no.hijos = 3, edades.hijos = c(4, 7, 9)))

#acceder a los datos de mi lista con $
milista$edades.hijos

#propiedades de la lista
str(milista)

#creando data frames
x <- 6:8
x
y <- c("A", "B", "C")

## asi veo el resultado si lo pongo en ()
(mifile <- data.frame(edad = x, grupo = y))
mifile

str(mifile)

#Extrayendo información del df, se hace igual que las matrices
  mifile[1]
  mifile[,1] # x columna
  mifile[1,] # x fila
  
#acceder a objetos, es el mismo resultado
  mifile$edad
  
#calcular estadísticos básicos
mean(mifile$edad)

#concatenar elemento, para agregar un mensaje
paste("La media de la edad es",mean(mifile$edad))

#inspeccionar a detalle "summary"
summary(mifile)

# conocer la dimensión
dim(mifile) #nos dice por cuantas filas y columnas esta compuesto

#agregar una columna
mifile$sexo <- c("H","M", "M")

#elimiar columna
mifile$sexo <- NULL
mifile

#Lectura de cuadros de datos





