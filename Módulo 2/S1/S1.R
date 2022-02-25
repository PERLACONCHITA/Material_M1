# creamos una variable
(var.number <-5L)
(var.logical<-F)
(var.double <- 2.7076)
(vector <- c(1,2))
(var.hola <- "Hola Mundo")


# ver la variable
view(var.logical)
view(vector)

# vemos que tipo de variable es
class(var.number)
class(var.hola)
class(var.double)
class(var.logical)

# cual es la diferencia entre class y typeof

class(var.double)
typeof(var.double)

# en este paso creamos vectores
a <- c(4, 6, 8, 10,12)
b <- c(3, 5, 7, 9)
c=45

# cuando es una variable simple =, aunque <- funcionan bien
# Para saber la longitud del vector

length(a)
length(b)

#Se puede acceder a las coordenadas de un vector haciendo uso [] y su posición
a[1]
b[2]

# para unir 2 vectores
c=c(a,b)

# organizarlos con sort
sort(c, decreasing = F)

sort(c(a,b), decreasing = T)

sort(c(4,6,8,10,12,3,5,7,9),decreasing = T)

# generar un vector corrido
vector.cien=1000:1050

view(vector.cien)

# vectores sin continuidad
(vector.by3 <- seq(1, 10, 3))

rep(a,2)
rep(b,3)
rep(c,100)

#reciclaje
c(1,2) + c(7,8,9,10)

# hacer las operaciones
a+b
a-b
