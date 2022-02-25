
library(ggplot2)



#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("sinhrks/ggfortify")

1

#Importa los datos de producción de electricidad que se encuentra en el archivo cbe.csv a R

url3.2 <- "https://raw.githubusercontent.com/beduExpert/Programacion-con-R-Santander/master/Sesion-03/Reto-03/cbe.csv"
download.file(url = url3.2, destfile = "cbe.csv", mode = "wb" )
cbe<- read.csv("cbe.csv")
View(cbe)

cbe[ ,3]

#Crea la serie de tiempo mensual de producción de electricidad en R a partir del año 1958
# Para hacer series de tiempo en R. Necesitamos un tipo de objeto que se llama ts (por time series)
cbe.elec <- ts(cbe$elec, start = 1958, #start la fecha de la primera observación
               frequency = 12) #frequency es la periocidad de la serie, el 12 indica que es mensual

cbe.elec

#Realiza la descomposición multiplicativa de la serie de tiempo y grafica la serie original junto con sus componentes (tendencia, estacionalidad y componente aleatoria)

cbe.elec.des <- decompose(cbe.elec, type = "multiplicative")
plot(cbe.elec.des)

#Realiza la gráfica de tendencia
#coloca la gráfica de tendencia x estacionalidad superpuesta a esta

?ts

radom.cbe <-cbe.elec.des$random
trend.cbe <- cbe.elec.des$trend
seasonal.cbe <-cbe.elec.des$seasonal

trendseasonal <- cbe.elec.des$trend*cbe.elec.des$seasonal
plot(trendseasonal)

ts.plot(trend.cbe, trendseasonal)



