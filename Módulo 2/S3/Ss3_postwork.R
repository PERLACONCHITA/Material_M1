#Con el �ltimo data frame obtenido en el postwork de 
#la sesi�n 2, elabora tablas de frecuencias relativas 
#para estimar las siguientes probabilidades:
#La probabilidad (marginal) de que el equipo que juega 
#en casa anote x goles (x=0,1,2,)

p1 <- table(t1$FTAG, exclude = c(3,4,5,6,7,8))

View(p1)
  
#La probabilidad (marginal) de que el equipo que juega 
#como visitante anote y goles (y=0,1,2,)

#La probabilidad (conjunta) de que el equipo que juega
#en casa anote x goles y el equipo que juega 
#como visitante anote y goles (x=0,1,2,, y=0,1,2,)

#Realiza lo siguiente:
#Un gr�fico de barras para las probabilidades 
#marginales estimadas del n�mero de goles que
#anota el equipo de casa


#Un gr�fico de barras para las probabilidades 
#marginales estimadas del n�mero de goles que 
#anota el equipo visitante.



#Un HeatMap para las probabilidades conjuntas 
#estimadas de los n�meros de goles que anotan 
#el equipo de casa y el equipo visitante en un partido.