#libraries
library(dplyr)
library(tidyverse)
library(ggplot2)

#Recuperar el archivo
booking <- read.csv("c:/Users/52333/Dropbox/Mi PC (LAPTOP-569HTKOE)/Documents/Prueba Git/TECP0013ADDAOL2112/Módulo 2/checkpoint/hotel_bookings.csv")
summary(booking)
dim(booking)
head(booking)
str(booking)
View(booking)

(colSums(is.na(booking)))

#cambiar a formato fecha reservation_status_date
bforest <- mutate(booking, reservation_status_date = as.Date(reservation_status_date, "%d/%m/%Y"),
                  is_canceled = factor(is_canceled))

b1 <- mutate(booking, reservation_status_date = as.Date(reservation_status_date, "%d/%m/%Y"),
             hotel = factor(hotel),
             is_canceled = factor(is_canceled),
             market_segment = factor(market_segment))

b3 <- filter(booking, is_canceled == 1)
b3 <- mutate(booking, reservation_status_date = as.Date(reservation_status_date, "%d/%m/%Y"),
             hotel = factor(hotel),
             is_canceled = factor(is_canceled))

tail(b1)


# Pernoctación por tipo de mercado
b1 %>%
  ggplot(aes(x= market_segment, y = stays_in_week_nights, fill=  hotel))+
           geom_boxplot()

#Cancelación por tipo de mercado
b1 %>%
  ggplot(aes(x=market_segment, y = stays_in_week_nights, fill = is_canceled))+
  geom_boxplot()

b1 %>%
  ggplot(aes(x=country, y = stays_in_week_nights, fill = is_canceled))+
  geom_boxplot()

adr_normal <- b1$adr / (b1$adults + b1$children)

b1 %>%
  ggplot(aes(x=reserved_room_type, y = adr_normal, fill = hotel))+
  geom_boxplot()+
  facet_wrap("is_canceled") +
  ylim(0,250)



#frecuencia de tipo de segmento de mercado
freq_tab= table(b1$market_segment)
freq_tab
barplot(freq_tab)

plot(b1$is_canceled)

hist(b1$lead_time)

#Si existe correlación entre tipo de segmento y el que cancelen reservación
attach(booking)
t1 <- table(deposit_type, is_canceled)
plot(t1)

#Prueba de shapiro

chisq.test(t1, simulate.p.value = TRUE)

#agrupar por fecha
b2 <- aggregate(b1["stays_in_week_nights"], by=b1["reservation_status_date"], sum)           
View(b2)  

#agrupar por lead_time
grouplead_time <- aggregate(booking["is_canceled"], by=booking["lead_time"], mean)
grouplead_time

grouplead_time %>%
  ggplot(aes(x=lead_time, y = (is_canceled), color = is_canceled))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

#agrupar por deposit_type
groupdeposit_type <- aggregate(booking["is_canceled"], by=booking["deposit_type"], mean)
groupdeposit_type

plot(factor(groupdeposit_type$deposit_type), groupdeposit_type$is_canceled)

groupdeposit_type %>%
  ggplot(aes(x= factor(deposit_type), y = is_canceled))+
  geom_bar(stat = "identity")


#agrupar por adr
groupadr <- aggregate(booking["is_canceled"], by=booking["adr"], mean)
groupadr

groupadr %>%
  ggplot(aes(x=adr, y = (is_canceled)))+
  geom_point()+
  xlim(0,500)+
  geom_smooth(method = "lm", se = FALSE)

#agrupar por market_segment
groupmarket_segment <- aggregate(booking["is_canceled"], by=booking["market_segment"], mean)
groupmarket_segment

plot(factor(groupmarket_segment$market_segment), groupmarket_segment$is_canceled)

groupmarket_segment %>%
  ggplot(aes(x= factor(market_segment), y = is_canceled))+
  geom_bar(stat = "identity")
  

#agrupar por total_specil_request
grouprequest <- aggregate(booking["is_canceled"], by=booking["total_of_special_requests"], mean)
grouprequest


grouprequest %>%
  ggplot(aes(x= factor(total_of_special_requests), y = is_canceled))+
  geom_point()



#Correlación de los datos
attach
b3 <- as.data.frame(lapply(booking, as.numeric))

summary(b3)

attach(booking)


b4 <- lm(is_canceled  ~ market_segment)
summary(b4)

contrasts(booking$hotel)

#matriz para 



library(radomForest)
install.packages("radomForest")

randomForest(booking)

urlPackage <- "https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-12.tar.gz"
install.packages(urlPackage, repos=NULL, type="source") 
library(randomForest)


set.seed(101)
tamano.total <- nrow(bforest)
tamano.entreno <- round(tamano.total*0.7)
datos.indices <- sample(1:tamano.total , size=tamano.entreno)
datos.entreno <- bforest[datos.indices,]
datos.test <- bforest[-datos.indices,]

modelo <- randomForest(is_canceled~., data=datos.entreno)
modelo

modelo3 <- randomForest(is_canceled~., data=datos.test)
modelo3

#opcion 2 con valor ajustado
modelo2 <- randomForest(is_canceled ~ ., data = datos.entreno, mtry = ncol(datos.entreno) - 2)
modelo2

#opcion 3 ajustar el valor

mtry <- tuneRF(bforest[-2], bforest$is_canceled, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

#gráficos y resultados
varImpPlot(modelo3)
plot(modelo)
legend("right", colnames(modelo$err.rate), lty = 1:5, col = 1:6)
importance(modelo2)

#install.packages("pdp")
#library(pdp)
#pdp1 <- partial(modelo, pred.var = "reservation_status", ice = TRUE)
#p1 <- plotPartial(pdp1)
#pdp2 <- partial(rf, c("density"))
#p2 <- plotPartial(pdp2)
#grid.arrange(p1, p2, ncol = 2)

#separar los árboles
# View(getTree(bagtrees, 1, labelVar=TRUE))
split_var_1 <- sapply(seq_len(modelo$ntree),
                      function(i) getTree(modelo, i, labelVar=TRUE)[1, "split var"])
table(split_var_1)

pred <- predict(modelo2, newdata = datos.test)
caret::confusionMatrix(pred, datos.test$is_canceled)


########## SVM
# Partición de los datos
set.seed(1)
df <- bforest
nobs <- nrow(bforest)
itrain <- sample(nobs, 0.8 * nobs)
train <- df[itrain, ]
test <- df[-itrain, ]

#Instalar paquete
install.packages("kernlab")
library(kernlab)
library(e1071)
set.seed(1) 

# Selección de sigma = mean(sigest(taste ~ ., data = train)[-2]) # depende de la semilla
#svm4 <- ksvm(is_canceled ~ ., data = train,
#            kernel = "rbfdot", prob.model = TRUE)
#svm
#summary(svm)

#svm predicción
#pred <- predict(svm, test)
#caret::confusionMatrix(pred, test$is_canceled)

#svm con variables fijas
#svm1 <- ksvm(is_canceled ~ lead_time + deposit_type, data = train)
#svm1
#summary(svm1)

##
set.seed(2022)
train = sample(nrow(bforest), 
               round(nrow(bforest)/2))
tail(bforest[train, ])


best <- svm(is_canceled~.,  data = bforest[train,],
            kernel = "radial",
            cost = 100,
            gamma = 1.51
)

best
summary(best)

#hacer plot


# svm con lead time y adr
best2 <- svm(is_canceled ~ lead_time + adr,  data = train,
            kernel = "radial",
            cost = 100,bes
            gamma = 1.51
)

best2
summary(best2)

names(best2)
plot(best2, test, lead_time ~ adr)
table(train$is_canceled, fitted(best2), dnn = c("Actual", "Predicho"))
pred <- predict(best2, test)
pred
table(test$is_canceled, pred)


paste("Observaciones de test mal clasificadas:", 
      100 * mean(test$is_canceled != pred) %>% 
        round(digits = 4), "%")







#tuning
set.seed(2)
tune.out <- tune(svm, is_canceled ~ lead_time + adr, data = train,
     kernel = "radial",
     ranges = list(
       cost = c(0.1, 1, 10, 100, 1000),
       gama = c(0.5, 1, 2, 3, 4)
     ))

#library(caret)
#predvals <- extractPrediction (list(pred), testx = adr, testy = lead_time)

#ajustes AUN NO QUEDAN
#mc <- table(true = bforest[-train, "bforest"], 
            pred = predict(best2, 
                           newdata = bforest[-train,]))
#mc

# El porcentaje total de aciertos obtenido por el modelo usando el 
# conjunto de prueba es el siguiente

round(sum(diag(mc))/sum(colSums(mc)), 5)

# Ahora observemos las siguientes proporciones

rs <- apply(mc, 1, sum)
r1 <- round(mc[1,]/rs[1], 5)
r2 <- round(mc[2,]/rs[2], 5)
rbind(No=r1, Yes=r2)

plot(best, bforest)

#set.seed(325)
#tuning <- tune(e1071::svm, is_canceled ~ ., data = train, 
 #              kernel = "linear")


#regresion logistica con todas las variables
rl <- glm(is_canceled ~., data = train)
rl
summary(rl)

#Regresión logística con las variables de random forest
rl2 <- glm(is_canceled ~ deposit_type + country + lead_time + market_segment + adr, family = binomial, data = train)
rl2
summary(rl2)

#ajuste de la regresión logística sin la variable que no aporta
rl3 <- update(rl2, ~. -country)
summary(rl3)


rl4 <- update(rl3, ~. +total_of_special_requests)
summary(rl4)
