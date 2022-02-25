# Obtén una mejor idea de las características de los data frames al usar las funciones: str, head, View y summary
t1 <- read.csv("SP1-1011.csv")
View(t1)
t2 <- read.csv("SP1-1112.csv")
t3<-read.csv("SP1-1213.csv")

str(t1)
getwd()

library(dplyr)
library(lubridate)
t1 <- select(t1, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

t1 <- mutate(t1, Date = as.Date(Date, "%d/%m/%Y"))
View(t1)

rbind(t1)
     