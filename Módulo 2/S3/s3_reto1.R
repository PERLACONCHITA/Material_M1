url <- "https://raw.githubusercontent.com/beduExpert/Programacion-con-R-2021/main/Sesion-03/Reto-01/BD_Altura_Alunos.csv"
download.file(url = url, destfile = "BD_Altura_Alunos.csv", mode = "wb" )
altura <- read.csv("BD_Altura_Alunos.csv")

View(altura)

altura_alumnos <- read.csv(file.choose())

View(altura_alumnos)
