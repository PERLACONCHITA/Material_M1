str(mtcars)
summary(mtcars)
head(mtcars, n=4)
View(mtcars)

x<-c(8,5,3,2,1)
length(x)

mediana <- function(vector){
  vector <- sort(vector)
  n <- length(vector)
  if (n%%2==0) {
    med = mean(vector[c(n/2, n/2+1)])
  } else {
    med = vector[round(n/2)]
  }
  paste("La mediana es:",med)
}

1%%2


mediana(x)


mediana <- function()
  
  norm <- function(vector){
    l <-  length(vector)
    
    if(l/2 == trunc(l/2)){
      (sort(vector)[l/2]+sort(vector)[(l/2)+1])/2
    }
    else{
      sort(vector)[(l+1)/2]
    }}