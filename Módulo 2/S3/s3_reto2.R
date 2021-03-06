url3.1 <- "https://raw.githubusercontent.com/beduExpert/Programacion-con-R-Santander/master/Sesion-03/Reto-02/players_stats.csv"
download.file(url = url3.1, destfile = "players_stats.csv", mode = "wb" )
players_stats<- read.csv("players_stats.csv")
View(players_stats)
library(dplyr)
library(ggplot2)

#Generar un histograma de los minuntos totales (MIN), de los jugadores y agregar una l�nea donde se muestre la media (Hint: para agregar la l�nea que muestre la media consulta la documentaci�n sobre geom_vline y el argumento xintercept)
meanminutes <- mean(players_stats$MIN)
meanminutes

players_stats%>%
  ggplot()+
  aes(MIN)+
  geom_histogram(binwidth = 60, col ="white", fill = "blue")+
  geom_vline(xintercept = meanminutes, size=1, col = "red")+
  ggtitle("Minutos totales de los jugadores")+
  xlab("Minutos")+
  ylab("Frecuencia")+
  theme_bw()


#Generar un histograma de edad (Age) y agregar una l�nea con la media
#players_stats <- na.omit(players_stats$Age)

meanage <- mean(na.omit(players_stats$Age))
meanage

players_stats%>%
  ggplot()+
  aes(Age)+
  geom_histogram(binwidth = 2, col ="white", fill = "blue")+
  geom_vline(xintercept = meanage, size=1, col = "red")+
  ggtitle("Edad de los jugadores")+
  xlab("Edad")+
  ylab("Frecuencia")+
  theme_bw()

#Hacer un scatterplot de las variables Weight y Height y observar la correlac�n que existe entre ambas variables (1 sola gr�fica)
players_stats2 <- na.omit(players_stats)
players_stats2%>%
  ggplot(aes(x = Weight, y = Height))+
  geom_point()+
  geom_smooth(method = "lm", se = T)+
  ggtitle("Peso y altura de los jugdores")

cor.test(players_stats$Weight, players_stats$Height)


#Utiliza la funci�n which.max para saber qui�n es el jugador m�s alto, una vez hecho esto, presenta los resultados en una leyenda que diga "El jugador m�s alto es: Name, con una altura de: Height". Las unidades de altura deben ser en metros.
highest<-which.max(players_stats2$Height)/100
highest

paste("El jugador m�s alto es:", players_stats2$Name[highest], "con una altura de", highest, "metros")

players_stats2$Height
which.max(players_stats2$Height)/100

View(players_stats2)

#Utiliza la funci�n which.min para saber qui�n es el jugador m�s bajito, una vez hecho esto, presenta los resultados en una leyenda que diga "El jugador m�s bajito es: Name, con una altura de: Height". Las unidades de altura deben ser en metros.
smaller <- which.min(players_stats2$Height)/100
smaller

paste("El jugador m�s bajo es:", players_stats2$Name[smaller], "con una altura de", smaller, "metros")

#�Cu�l es la altura promedio?, representa el resultado en una frase que diga: "La altura promedio es: ALTURA"
(meanhigh <- mean(players_stats2$Height)/100)
paste("La altura promedio es:", meanhigh, "metros")


#Generar un scatterplot donde se representen las Asistencias totales (AST.TOV) vs Puntos (PTS), adem�s has un face wrap con la posici�n (Pos).
players_stats%>%
  ggplot(aes(x = AST.TOV, y= PTS))+
  geom_point()+
  facet_wrap("Pos")+
  ggtitle("Asistencias totales y puntos seg�n la posici�n")
  xlab("Asistencias totales")+
  ylab("Puntos")+
  theme_light()

