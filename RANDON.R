#####MODELO DE RAMDON FOREST
#Es un método no paramétrico usado para clasificación y regresión, la idea básica es que un nuevo 
#caso se va a clasificar según la clase que tengan sus K - Vecinos más cercanos. Es un concepto 
#simple , intuitivo y fácil de implementar por eso es un método de uso común.
#cualitativa dependiente.
#####PREDEDIR CUAL ES PROBABILIDAD QUE UN VEHICULO NO TENGA FALLOS
#####EN UN RECORRIDO CON UN OPTIMO MANTENIMIENTO 
#####ANALISIS PREDICTIVO
#####SE UTILIZA COMO VARIABLE DEPENDIENTE FALLO_VEHICULO
########################################################################
######CONFIGURACION DE LIBRERIAS

install.packages("ggplot2")
install.packages("randomForest")
install.packages("stats")
install.packages("prediction")
install.packages("abind")
install.packages("sf")
install.packages("pROC")
install.packages("mosaicData")
install.packages("vcd")
install.packages("grid")
library(stats)
library(ggplot2)
library(purrr)
library(dplyr)
library(pROC)
library(randomForest)
library(mosaic)
library(pROC)
library(mosaicData)
library(grid)
library(vcd)
library(abind)
library(sf)


########################################################################
######CARGUE DEL ARCHIVO Y REVISION DE DATOS
######38848 - 20 VARIABLES

R_EQUIRENT_F <- read.csv("C:/Users/GIT_sistemas/Desktop/R_EQUIRENT_FN.csv", sep=";")
####################################################################################################
######SELECCION DE UNA MUESTRA DEL 60 % Y 40% DE PRUEBA DE ENTRENAMIENTO 
datos<-R_EQUIRENT_F
muestra <- sample(1:2361, 944)
test <- datos[muestra, ]
train <- datos[-muestra, ]

#DATOS DE PRUEBA
dim(test)[1]

######DATOS DE ENTRENAMIENTO
dim(train)[1]

######CONVERSION DE VARIABLES FACTORIALES A NUMERICAS
datos$FALLO_VEHICULO <-as.factor(datos$FALLO_VEHICULO)
datos$PROMEDIO_MAX_VELOCIDAD <-as.numeric(datos$PROMEDIO_MAX_VELOCIDAD)
datos$PROMEDIO_COMBUSTIBLE <-as.numeric(datos$PROMEDIO_COMBUSTIBLE)
datos$PROMEDIO_TRASMISION <-as.numeric(datos$PROMEDIO_TRASMISION)
datos$CAMBIO_LLANTAS <-as.numeric(datos$CAMBIO_LLANTAS)
datos$CALIBRACION_NEUMATICOS <-as.numeric(datos$CALIBRACION_NEUMATICOS)
datos$SINCRONIZACION <-as.numeric(datos$SINCRONIZACION)
datos$ALINEACION_BALANCEO <-as.numeric(datos$ALINEACION_BALANCEO)
datos$REVISION_FRENOS <-as.numeric(datos$REVISION_FRENOS)
####################################################################################################
###### MODELO DE RAMDON FOREST CON  22 VARIABLES EXCEPTO GRADO MENOR Y MAYOR
suppressWarnings(suppressMessages(library(kknn)))
mKnn <- train.kknn(as.factor(FALLO_VEHICULO)~
                     CANTIDAD_EXCESOS_VELOCIDAD+PROMEDIO_MAX_VELOCIDAD+
                     PROMEDIO_TRASMISION+
                     CANTIDAD_HORAS_TRABAJO+
                     CANTIDAD_BATERIA_RESPALDO+CAMBIO_LLANTAS+CALIBRACION_NEUMATICOS+
                     CAMBIO_ACEITE+
                     SINCRONIZACION+ALINEACION_BALANCEO, 
                   data = train, kmax = 6)#CAMBIAR A 8

mKnn
summary(mKnn)
plot(mKnn)


####################################################################################################
######PREDICCION PARA DATOS DE ENTRENAMIENTO
pred <- predict(mKnn, test[, -20])
resul<-(mc <- with(test,table(pred, as.factor(FALLO_VEHICULO),dnn = c("observaciones", "predicciones"))))
head(resul)

######MOSAICO DE LAS PREDICCIONES
library(mosaic)
mosaic(resul, shade = T, colorize = T, 
       gp = gpar(fill = matrix(c("green3", "darkcyan", "darkcyan", "green3"), 2, 2)))
#####################################################################################################
######USAN LA FUNCION CARET SE CALCULA LA PREDICCION - ESPECIFICIDAD , SENSIBILIDAD
CM=caret::confusionMatrix(as.factor(pred),as.factor(test[, 20]),positive="1",mode="everything")
CM

######CALCULO DE ERROR
error=(table(as.factor(pred),test[, 20])[1,2]+table(as.factor(pred),
                                                    test[, 20])[2,1])/sum(table(as.factor(pred),test[, 20]))
error

#####################################################################################################
######RESULTADO PREDICCION
# % correcto
paste("La probabilidad de personas que cursen grado 11 con el modelo de vecinos mas cercanos es del",
      100 * sum(diag(mc)) / sum(mc))#84,43%

#####################################################################################################
######GRAFICOS DEL MODELO DE VECINOS MAS CERCANOS

#####GRAFICA DE CURVA DE ROC
#predict.Knn<- predict(mKnn,test,type = "prob") #prob. clase=yes
#roc1=roc(test[, 22], predict.Knn[,2])
#ggroc(roc1,col="green")

predict.knn<- predict(mKnn,test,type = "prob") #prob. clase=yes
roc1=roc(test$FALLO_VEHICULO,predict.knn[,2])
plot.roc(smooth(roc1),col="darkcyan", print.auc=TRUE,main = "Curva de ROC para Modelo de KNN")

