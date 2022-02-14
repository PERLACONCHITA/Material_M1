#Función str. Muestra de manera compacta
# la estructura de un objeto
str(iris)

#Summary: resumenes de datos
summary(1:100)
summary(mtcars)


set.seed(57)
x <- rnorm(35)
e <- rnorm(35)
y <- 5 + 2*x + e #se opera por cada valor de x y cada e por eso salen muchos valores, porque lo hace de los 35
View(y)
y
modelo <- lm(y~x)
summary(modelo)

# head la primera parte del data frame
head(mtcars) #6 filas

#tail la última parte
tail(mtcars, n=1000) #6 filas, con la n decimos cuantos datos queremos ver

#View es el visor
View(iris)

x1=c(1,2,3,4,5,2,5,5)
sum(x1)


table(x1)
max(x1)
sort(x1)
length(x1)


# creando nuestras propias funciones
moda <- function(vector){
  f.abs <- table(vector) # frecuencias absolutas
  max.f.abs <- max(f.abs) # obtenemos la máxima frecuencia absoluta
  pos.max <- which(f.abs == max.f.abs) # posición(es) de la(s) máxima(s) frecuencia(s) absoluta(s)
  print("La(s) moda(s) es(son): ")
  print(names(f.abs[pos.max]))
  paste("Con una frecuencia de: ", unique(as.vector(f.abs[pos.max])))
}


view(moda)
moda(x)

x <- sample(1:100, 100, replace = T) # Tomamos una muestra aleatoria de tamaño 100 con reemplazo de los primeros 100 números naturales
table(x) # obtenemos las frecuencias absolutas de los valores de la muestra
moda(x) # obtenemos la moda de los valores de la muestra
