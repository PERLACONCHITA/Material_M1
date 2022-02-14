# cbind toma una sucesion de elementos
#(vectores, matrices, dataframes) y los
#combina en columnas

(cbind(1:10, 11:20, 21:30))
cbind(1:10, matrix(11:30, ncol =2))
cbind(data.frame(x = 1:10, y = 11:20), z = 21:30)

#funcion rbind los combina por filas
(rbind(1:10, 11:20, 21:30))


df1 <- data.frame(x = 1:5, y = 6:10, z = 16:20)
df2 <- data.frame(x = 51:55, y = 101:105, z = 151:155)
df1; df2
rbind(df1, df2)
