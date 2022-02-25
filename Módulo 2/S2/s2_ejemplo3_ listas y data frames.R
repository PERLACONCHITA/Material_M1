# descargar paquete dplyr sin mensajes ni advertencias
suppressMessages(suppressWarnings(library(dplyr)))


#descargr datos url
url1 <- "https://data.humdata.org/hxlproxy/data/download/time_series_covid19_confirmed_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv"
url2 <- "https://data.humdata.org/hxlproxy/data/download/time_series_covid19_deaths_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv"

# en nuestro directorio
download.file(url = url1, destfile = "st19ncov-confirmados.csv", mode = "wb")
download.file(url = url2, destfile = "st19ncov-muertes.csv", mode = "wb")

#importamos los datos
conf <- read.csv("st19ncov-confirmados.csv") #confirmados
mu <- read.csv("st19ncov-muertes.csv")       #muertes

conf

#vemos sus características
str(conf)
str(mu)
head(conf)
head(mu)

#seleccionamos todas las filas excepto la primera
Sconf <- conf[-1, ]
Smu <- mu[-1, ]
Sconf

Sconf2 <- conf[-1:-100,] #aquí elimino conjunto de filas

# con la funcion select del paquete dplyr
#del data frame casos confirmados seleccionadmos
#pais, fecha y numero acumulado de casos
Sconf <- select(Sconf, Country.Region, Date, Value) # País, fecha y acumulado de infectados
Sconf
View(Sconf)

#renombramos las columnas correspondientes
Sconf <- rename(Sconf, Pais = Country.Region, Fecha = Date, Infectados = Value)
str(Sconf)
View(Sconf)

# las columnas aparecen como factor, vamos a transformar
# a fechas y a números, función mutate
Sconf <- mutate(Sconf, Fecha = as.Date(Fecha, "%Y-%m-%d"), Infectados = as.numeric(Infectados)) 
View(Sconf)

#lo mismo para el data frame de muertos
Smu <- select(Smu, Country.Region, Date, Value) # Seleccionamos país, fecha y acumulado de muertos
Smu <- rename(Smu, Pais = Country.Region, Fecha = Date, Muertos = Value) # Renombramos
Smu <- mutate(Smu, Fecha = as.Date(Fecha, "%Y-%m-%d"), Muertos = as.numeric(Muertos)) 
View(Smu)

#vamos a unir acumulados con muertos por fecha
Scm <- merge(Sconf, Smu)
View(Scm)

# seleccionamos solo mexico y primer dia de infectados
mex <- filter(Scm, Pais=="Mexico")
mex <- filter(mex, Infectados != 0)

View(mex)

  # Nuevos infectados por día
mex <- mutate(mex, NI = c(1,diff(Infectados))) #se pone el uno porque con ese se inicia

# Nuevos muertos por día
mex <- mutate(mex, NM = c(0, diff(Muertos)))

# Tasa de letalidad
mex <- mutate(mex, Letalidad = round(Muertos/Infectados*100, 1))
tail(mex)

# Valores del día anterior es la función lag
mex <- mutate(mex, IDA = lag(Infectados), MDA = lag(Muertos))

# Factores de crecimiento
mex <- mutate(mex, FCI = Infectados/IDA, FCM = Muertos/MDA)

#Días de contingencia
mex <- mutate(mex, Dia = 1:dim(mex)[1])

#vemos las filas
head(mex)
View(mex)
tail(mex)

write.csv(mex, "C19Mexico.csv", row.names = FALSE)
