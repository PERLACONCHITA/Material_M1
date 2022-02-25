# cargar el dataset
url <- "https://raw.githubusercontent.com/beduExpert/Programacion-con-R-2021/main/Sesion-03/Reto-01/BD_Altura_Alunos.csv"
download.file(url = url, destfile = "BD_Altura_Alunos.csv", mode = "wb" )
altura <- read.csv("BD_Altura_Alunos.csv")

View(altura)

altura_alumnos <- read.csv(file.choose())

View(altura_alumnos)
names(altura_alumnos)

#histograma
library(ggplot2)

hist(altura_alumnos$Altura, breaks = 5, col = "white",
     main = "Altura alumno",
     xlab = "Altura en centimetros",
     ylab = "Frecuencia de alumnos")

#histograma con ggplot
library(dplyr)
altura_alumnos%>%
  ggplot()+
  aes(Altura)+
  geom_histogram(binwidth = 5, col ="white", fill = "green")+
  ggtitle("Histograma de altura") +
  ylab("Frecuencia de alumnos") +
  xlab("Altura en centimetros") + 
  theme_minimal()
