#### T�cnicas descriptivas: gr�ficas, tendencias y variaci�n estacional

install.packages("TSA")

library(TSA)

data(oilfilters); 
View(oilfilters)

str(oilfilters)

plot(oilfilters)

plot(oilfilters, type = "b")

plot(oilfilters, type = "o", 
     ylab = "Ventas", 
     xlab = "Tiempo", 
     main = "Ventas Mesuales ")
  
plot(oilfilters, 
     type = "l", 
     ylab = "Ventas", 
     xlab = "Tiempo",
     main = "Ventas Mensuales de Filtro de Aceite",
     sub = "S�mbolos Especiales")

points(y = oilfilters, 
       x = time(oilfilters),
       pch = as.vector(season(oilfilters)))

(season(oilfilters)) #para ver como est�n dados los datos en el tiempo

data(AirPassengers)
AP <- AirPassengers
AP

# Clase de un objeto

class(AP)

start(AP); 
end(AP); 
frequency(AP)

summary(AP)

plot(AP, 
     ylab = "Pasajeros (1000's)", 
     xlab = "Tiempo", 
     main = "Reserva de pasajeros a�reos internacionales", 
     sub = "Estados Unidos en el periodo 1949-1960")

################################################

layout(1:2)
plot(aggregate(AP), 
     xlab = "Tiempo",
     main = "Reserva de pasajeros a�reos internacionales", 
     sub = "Estados Unidos en el periodo 1949-1960")

boxplot(AP ~ cycle(AP),
        xlab = "Boxplot de valores estacionales",
        sub = "Estados Unidos en el periodo 1949-1960",
        main = "Reserva de pasajeros a�reos internacionales")
dev.off()

################################################

# https://github.com/AtefOuni/ts/tree/master/Data

# Series de Tiempo M�ltiple

# Serie de producci�n de electricidad, cerveza y chocolate

CBE <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-con-R-2021/main/Sesion-06/Ejemplo-01/cbe.csv", header = TRUE)
View(CBE)
CBE[1:4,]
class(CBE)

#AQU� ESTOY HACIENDO MIS SERIES DE TIEMPO
Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)
Beer.ts <- ts(CBE[, 2], start = 1958, freq = 12)
Choc.ts <- ts(CBE[, 1], start = 1958, freq = 12)


View(CBE)

plot(cbind(Elec.ts, Beer.ts, Choc.ts), 
     main = "Producci�n de Chocolate, Cerveza y Electricidad", 
     xlab = "Tiempo",
     sub = "Enero de 1958 - Diciembre de 1990")

################################################

# Serie de temperaturas globales, expresadas como anomal�as de las medias mensuales

Global <- scan("global.txt")
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12), fr = 12)
Global.annual <- aggregate(Global.ts, FUN = mean)
plot(Global.ts, xlab = "Tiempo", ylab = "Temperatura en °C", main = "Serie de Temperatura Global",
     sub = "Serie mensual: Enero de 1856 a Diciembre de 2005")
plot(Global.annual, xlab = "Tiempo", ylab = "Temperatura en °C", main = "Serie de Temperatura Global",
     sub = "Serie anual de temperaturas medias: 1856 a 2005")

################################################

New.series <- window(Global.ts, start = c(1970, 1), end = c(2005, 12)) 
New.time <- time(New.series)
plot(New.series, xlab = "Tiempo", ylab = "Temperatura en °C", main = "Serie de Temperatura Global",
     sub = "Serie mensual: Enero de 1970 a Diciembre de 2005"); abline(reg = lm(New.series ~ New.time))


#### Descomposici�n de series

# Modelo Aditivo

# Se debe elegir entre el modelo aditivo o el modelo multiplicativo cuando sea razonable suponer la descomposici�n
Elec.decom.A <- decompose(Elec.ts)

Elec.decom.A$random

cuadro = data.frame(est = Elec.decom.A$random, obs = Elec.decom.A$x)
cuadro



plot(Elec.decom.A, xlab = "Tiempo", 
     sub = "Descomposici�n de los datos de producci�n de electricidad")

#pronostico
install.packages("forecast")
library(forecast)

#MODELO ARIMA

fit = auto.arima(Elec.ts)

summary(fit)

plot(forecast(fit, h=20)) #h=los meses que voy a proyectar


#ver los datos de la gr�fica
data.frame(proy = forecast(fit, h=20))



# Componentes

Tendencia <- Elec.decom.A$trend
Estacionalidad <- Elec.decom.A$seasonal
Aleatorio <- Elec.decom.A$random

ts.plot(cbind(Tendencia, Tendencia + Estacionalidad), 
        xlab = "Tiempo", main = "Datos de Producci�n de Electricidad", 
        ylab = "Producci�n de electricidad", lty = 1:2,
        sub = "Tendencia con efectos estacionales aditivos sobrepuestos")

Tendencia[20] + Estacionalidad[20] + Aleatorio[20]
Elec.ts[20]

###

# Modelo Multiplicativo

Elec.decom.M <- decompose(Elec.ts, type = "mult")

plot(Elec.decom.M, xlab = "Tiempo", 
     sub = "Descomposici�n de los datos de producci�n de electricidad")

# Componentes

Trend <- Elec.decom.M$trend
Seasonal <- Elec.decom.M$seasonal
Random <- Elec.decom.M$random

ts.plot(cbind(Trend, Trend*Seasonal), xlab = "Tiempo", main = "Datos de Producci�n de Electricidad", 
        ylab = "Producci�n de electricidad", lty = 1:2,
        sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")

Trend[7]*Seasonal[7]*Random[7]
Elec.ts[7]

Trend[100]*Seasonal[100]*Random[100]
Elec.ts[100]


# J. Cryer & K. Chan. (2008). Time Series Analysis With Applications 
# in R. 233 Spring Street, New York, NY 10013, USA: Springer 
# Science+Business Media, LLC.

# P. Cowpertwait & A. Metcalfe. (2009). Introductory Time Series with R. 
# 233 Spring Street, New York, NY 10013, USA: Springer Science+Business 
# Media, LLC.