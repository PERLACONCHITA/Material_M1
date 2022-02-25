library(ggplot2)
test10 <- theme_bw(base_line_size = base_size/11)
base_size <- 11
test10 <- theme_bw(base_size = base_size, base_line_size = base_size/11)

#para deshabilitar una biblioteca
#detach("package:ggplot2", unload = True)

# recordar las variables del dataset
names(mtcars)


#Graficamos la variable cyl en el eje x
#y hp en y

ggplot(mtcars, aes(x=cyl, y = hp, colour = mpg )) + 
geom_point()  # Tipo de geomería, intenta utilizar alguna otra

?geom_point



#agregando características de tema y face_wrap
ggplot(mtcars, aes(x=cyl, y = hp, colour = mpg))+
  geom_point()+
  theme_dark()+ #temas
  facet_wrap("cyl") #lo divide por el número de cilindros, por categorías

#facet_wrap no se usa para corte continuo

#cambiar de color
ggplot(mtcars, aes(x=cyl, y = hp, colour = mpg )) +
geom_point(aes(colour=factor(cyl)))

#cambiar forma
geom_point(aes(shape = factor(cly)))

#bubblechart
geom_point(aes(size = qsec))

#set aesthetics to fixed value
ggplot(mtcars, aes(wt, mpg)) + 
  geom_point(colour = "red", size = 3)

#agregando características de tema y facewrap
names(mtcars)
ggplot(mtcars, aes(x=cyl, y = hp, colour = mpg )) + 
  geom_point(aes(shape = factor(cyl))) +   
  theme_gray(
    base_size = 11,
    base_family = "",             #esto no tiene lógica por el tipo de información
    base_line_size = base_size/22,
    base_rect_size = base_size/22
  ) +   # Temas (inteta cambiarlo)
  facet_wrap("cyl")  # Lo divide por el núm de cilindros

?theme_gray

#agregando nombres a los ejes x,y

names(mtcars)
ggplot(mtcars, aes(x=cyl, y = hp, colour = mpg )) + 
  geom_point(aes(shape = factor(cyl))) +   
  theme_light() +   # Temas (inteta cambiarlo)
  facet_wrap("cyl") +  # Lo divide por el núm de cilindros
  xlab("Num Acilindros")+
  ylab("Caballos de Fuerza")
  