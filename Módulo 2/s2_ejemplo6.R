head(airquality)
library(dplyr)

# tipo de objeto y variables
str(airquality)

#dimension
dim(airquality)

#complete.cases veremos las filas que no tienen
#ningun valor perdido yy cuales tienen uno
bien <- complete.cases(airquality)
bien

#true es donde no hay NA'Sy False donde si hay

#el numero de filas donde no hay NA's
sum(bien)

#filtrar las que no tienen NA's
airquality[bien,]

#guardamos
data <- select(airquality, Ozone:Temp)
apply(data, 2, mean)
apply(data, 2, mean, na.rm = T)

#devuelve el objeto con casos incompletos
#eliminados
(m1 <- apply(na.omit(data), 2, mean))

b <- complete.cases(data)

(m2 <- apply(data[b,], 2, mean))

identical(m1, m2)


#otro ejemplo

x2 <- matrix(1:49, ncol = 7)
x2
(apply(x2,1,mean))
(apply(x2,2,median))

