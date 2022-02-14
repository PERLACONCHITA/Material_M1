view(SP1)
str(SP1)
?SP1
??SP1
class(SP1$FTHG)

summary(SP1) 

# La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x=0,1,2,)
# tabla de frecuencia x
table(SP1$FTHG, exclude = c(4,5,6,7))
tabla1 <- table(SP1$FTHG, exclude = c(4,5,6,7))
tabla1
View(tabla1)
barplot(tabla1)


#tabla de frecuencia relativa x
prop.table(x=tabla1)
propabilidad1 <- prop.table(x=tabla1)
View(propabilidad1)


#La probabilidad (marginal) de que el equipo que juega en casa anote y goles (y=0,1,2,)
# tabla de frecuencia 
tabla2 <- table(SP1$FTAG, exclude = c(4))
tabla2
View(tabla2)

#tabla de frecuencia relativa
prop.table(x=tabla2)
probabilidad2<-prop.table(x=tabla2)
View(probabilidad2)


# La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x=0,1,2,, y=0,1,2,)
# tabla de frecuencia de dos vías
prop.table(table(SP1$FTHG, SP1$FTAG, exclude = c(4,5,6)))

