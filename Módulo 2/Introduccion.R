install.packages("tidyverse")
install.packages("ggplot2")
library(ggplot2)
ggplot(mpg,aes(displ,hwy,colour=class))+geom_point()
library(profvis)
install.packages("dplyr")
install_github("tidyverse/dplyr")
install.packages("devtools")
devtools::install_github("tidyverse/dplyr")
library(dplyr)
starwars %>%
filter(species=="Droid")  
starwars %>% 
  select(name, ends_with("color"))
starwars %>% 
  mutate(name, bmi = mass / ((height / 100)  ^ 2)) %>%
  select(name:mass, bmi)
starwars %>% 
  arrange(desc(mass))

starwars %>%
  group_by(species) %>%
  summarise(
    n = n(),
    mass = mean(mass, na.rm = TRUE)
  ) %>%
  filter(
    n > 1,
    mass > 50
  )
(5+7)/2
(5+5)/2
9/1

mean(c(5,7))
mean(c(5,5))
  
saludo<-function(nombre){mensaje<-paste( "Hola",nombre)return(mensaje)}

saludo <- function(nombre){ mensaje <- paste("Hola", nombre) return(mensaje)}

operacion.personalizada <- function(a,b,c){ paso.1 <- (a+b)*4 + 2*a
paso.2 <- paso.1*2 + c/2 paso.3 <- paso.2*3 return(paso.3)
}

fahr_to_kelvin <- function(temp) {if (!is.numeric(temp)) {stop("temp must be a numeric vector.")}
  kelvin <- ((temp - 32) * (5 / 9)) + 273.15
  return(kelvin)}

library(tidyverse)
library(tidyverse/dplyr)

saludo<-function(nombre){mensaje<-paste( "Hola",nombre)return(mensaje)}
library(ggplot2)

saludo <- function(nombre){ mensaje <- paste("Hola", nombre) return(mensaje)}

(var.hola <-"Hola Mundo")
