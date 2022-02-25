# Generar vector de 44 entradas (aleatorias) llamado ran
ran<- sample(1:100,44)
ran
ran2<-ran[1:15]
ran2

#Loop que eleve al cubo las primeras 15 entradas y les sume 12
ran.resultado <- 0
for(i in 1:15) {
  ran.resultado[i]<- (ran2[i]**3)+12
  print(ran.resultado[i])
}

(df.al<- data.frame(ran2,ran.resultado))

#Escribe un pseucódigo del loop anterior
if(df.al[1,2]>6000){
  y <- TRUE
} else {
  y<-FALSE
}
y
