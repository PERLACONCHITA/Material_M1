#LOOPS

#Mediana

W <- rnorm(20)
print("Este loop calcula el cuadrado de los 10 primeros elementos del vector w")

#inicializando la variable wsq

wsq <-0

for(i in 1:10) {
  wsq[i] <- W[i]**2
  print(wsq[i])
}

for(i in seq(2:10, 5)) {
  wsq[i] <- W[i]**2
  print(wsq[i])
}



# los bucles WHILE comienzan comprobando una condición
# while()

count<-0
while(count <= 10) {
  print(count)
  count <- count + 1
}


#IF
#Pseucódigo if else

x <- runif(1, 0, 10) #creamos un número aleatorio entre 0 y 10

x
if(x > 5) {
  y <- TRUE
} else {
  y <- FALSE
}

x

y

x:y

#anadación de código / condición
#else if condición

x=99

if(x > 100) {
  y <- TRUE
}   else if (x < 100) {
      y <- FALSE
}


# indicamos de cuanto es el salto
for(i in seq(0, 10, by=3)){
  wsq[i] <- W[i]**2
  print(wsq[i]) 
}



