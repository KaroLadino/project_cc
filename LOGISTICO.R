#####REGRESION LOGISTICA
#La Regresión Logística multiple, desarrollada por David Cox en 1958, es un método de regresión que 
#estima la probabilidad de una variable cualitativa en función de una variable cuantitativa. Permite 
#estudiar en qué medida variaciones de una variable continua independiente influyen en una variable 
#cualitativa dependiente.
#####PREDEDIR CUAL ES PROBABILIDAD QUE UN VEHICULO NO TENGA FALLOS
#####EN UN RECORRIDO CON UN OPTIMO MANTENIMIENTO 
#####ANALISIS PREDICTIVO
#####SE UTILIZA COMO VARIABLE DEPENDIENTE FALLO_VEHICULO
########################################################################
######CONFIGURACION DE LIBRERIAS

install.packages("pROC")
install.packages("stats")
install.packages("caret")
install.packages("mosaic")
install.packages("Matrix")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("base")
install.packages("ggFormula")
install.packages("ggstance")
install.packages("mosaicData")
install.packages("vcd")
install.packages("grid")
install.packages("psych")
library(stats)
library(pROC)
library(caret)
library(lattice)
library(ggplot2)
library(mosaic)
library(Matrix)
library(base)
library(dplyr)
library(ggformula)
library(ggstance)
library(mosaicData)
library(grid)
library(vcd)
library(psych)


########################################################################
######CARGUE DEL ARCHIVO Y REVISION DE DATOS
######38848 - 20 VARIABLES

R_EQUIRENT_F <- read.csv("C:/Users/GIT_sistemas/Desktop/R_EQUIRENT_FN.csv", sep=";")

datos<-R_EQUIRENT_F
summary(R_EQUIRENT_F)

########################################################################
######SELECCION DE LA MUESTRA DEL 60% ENTRENAMIENTO Y 40% PRUEBA

datos<-R_EQUIRENT_F

test=sample(1:nrow(R_EQUIRENT_F),floor(nrow(R_EQUIRENT_F)*0.4),replace=FALSE)
train=sample(1:nrow(R_EQUIRENT_F),floor(nrow(R_EQUIRENT_F)*0.6),replace=FALSE)

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


########################################################################
######ANALISIS DE LA PRIMERA VARIABLE - PREDEDIR CUAL ES PROBABILIDAD QUE UN VEHICULO NO TENGA FALLOS
######EN UN RECORRIDO CON UN OPTIMO MANTENIMIENTO 
######SE UTILIZARON LAS VARIABLES MENOS SIGNIFICATIVAS


#modelo_logistico <- glm(formula=as.factor(FALLO_VEHICULO)~CANTIDAD_VIAJES+CANTIDAD_MESES+
#                      CANTIDAD_KM_RECORRIDOS+
#                      CANTIDAD_EXCESOS_VELOCIDAD+PROMEDIO_MAX_VELOCIDAD+CANTIDAD_FRENADAS+
#                     CANTIDAD_ACELERADAS+PROMEDIO_TRASMISION+PROMEDIO_COMBUSTIBLE+
#                     CANTIDAD_HORAS_TRABAJO+
#                     CANTIDAD_BATERIA_RESPALDO+CAMBIO_LLANTAS+CALIBRACION_NEUMATICOS+
#                     CAMBIO_ACEITE+
#                      SINCRONIZACION+ALINEACION_BALANCEO+REVISION_FRENOS, 
#                      data = datos[train, ], family = "binomial")
#summary(modelo_logistico)
#modelo_logistico


modelo_logistico1 <- glm(formula=as.factor(FALLO_VEHICULO)~
                          CANTIDAD_EXCESOS_VELOCIDAD+PROMEDIO_MAX_VELOCIDAD+
                          PROMEDIO_TRASMISION+
                          CANTIDAD_HORAS_TRABAJO+
                          CANTIDAD_BATERIA_RESPALDO+CAMBIO_LLANTAS+CALIBRACION_NEUMATICOS+
                          CAMBIO_ACEITE+
                          SINCRONIZACION+ALINEACION_BALANCEO, 
                        data = datos[train, ], family = "binomial")
summary(modelo_logistico1)
modelo_logistico1


########################################################################
######PREDICCION DE MODELO LOGISTICO

pred=format(round(predict(modelo_logistico1,datos[-train,],type="response")>0.7))
resul<-table(pred,datos[-train,20],dnn = c("observaciones", "predicciones"))
head(resul)


paste("La probabilidad que un vehiculo no tenga fallos en un recorrido con un optimo mantenimiento es",
      100 * sum(diag(resul)) / sum(resul)) #95.76%


error=(table(as.factor(pred),datos[-train,20])[1,2]+table(as.factor(pred),
       datos[-train,20])[2,1])/sum(table(as.factor(pred),datos[-train,20]))
error
#4.23%

######MOSAICO DE LAS PREDICCIONES
library(mosaic)
mosaic(resul, shade = T, colorize = T, 
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))



######USAN LA FUNCION CARET SE CALCULA LA PREDICCION - ESPECIFICIDAD , SENSIBILIDAD
CM=caret::confusionMatrix(as.factor(pred),as.factor(datos[-train,20]),positive="1",mode="everything")
CM


#####################################################################################################
######GRAFICOS DEL MODELO LOGISTICO # 1
library(stats)
library(pROC)
#####GRAFICA DE CURVA DE ROC
predict.rl<- predict(modelo_logistico1,datos[-train,],type = "response") #prob. clase=yes
roc1=roc(datos[-train,20], predict.rl)
plot.roc(smooth(roc1),col="blue", print.auc=TRUE,main = "Curva de ROC para Modelo Logistico")

#####################################################################################################
#####GRAFICA DE MODELO LOGISTICO EN FUNCION DE CANTIDAD_HORAS Y VAR_RES_11
MU2 <- glm(FALLO_VEHICULO ~ CANTIDAD_HORAS_TRABAJO, data = datos, family = "binomial")
plot(x = datos$CANTIDAD_HORAS_TRABAJO, y = datos$FALLO_VEHICULO , col = "darkblue",
     main = "probabilidad de Fallos en Vehiculos x Horas Trabajo", xlab = "Cantidad de Horas Trabajo",
     ylab = "Probabilidad de Fallo Vehiculo")
curve(predict(MU2, data.frame(CANTIDAD_HORAS_TRABAJO = x), type = "response"),
      add = TRUE, col = "firebrick", lwd = 2.5)

confint(object = MU2, level = 0.95 )


#MEDIANTE GGPLOT2 INCLUYENDO INTERVALOS DE CONFIANZA

datos$FALLO_VEHICULO <- as.character(datos$FALLO_VEHICULO)
datos$FALLO_VEHICULO <- as.numeric(datos$FALLO_VEHICULO)

#Se crea un vector con nuevos valores interpolados en el rango de observaciones.
nuevos_puntos <- seq(from = min(datos$CANTIDAD_HORAS_TRABAJO), to = max(datos$CANTIDAD_HORAS_TRABAJO),
                     by = 0.5)


#Predicciones de los nuevos puntos según el modelo. 
#Si se indica se.fit = TRUE se devuelve el error estándar de cada predicción
#junto con el valor de la predicción (fit).
predicciones <- predict(MU2, data.frame(CANTIDAD_HORAS_TRABAJO = nuevos_puntos),
                        se.fit = TRUE)

#Mediante la función logit se transforman los log_ODDs a probabilidades.
predicciones_logit <- exp(predicciones$fit) / (1 + exp(predicciones$fit))

#Se calcula el límite inferior y superior del IC del 95% sustrayendo e
#incrementando el logODDs de cada predicción 1.95*SE. Una vez calculados los
#logODDs del intervalo se transforman en probabilidades con la función logit.
limite_inferior       <- predicciones$fit - 1.95 * predicciones$se.fit
limite_inferior_logit <- exp(limite_inferior) / (1 + exp(limite_inferior))
limite_superior       <- predicciones$fit + 1.95 * predicciones$se.fit
limite_superior_logit <- exp(limite_superior) / (1 + exp(limite_superior))

#Se crea un data frame con los nuevos puntos y sus predicciones
datos_curva <- data.frame(CANTIDAD_HORAS_TRABAJO = nuevos_puntos,
                          probabilidad_FALLO_VEHICULO = predicciones_logit,
                          limite_inferior_logit = limite_inferior_logit, 
                          limite_superior_logit = limite_superior_logit)

ggplot(datos, aes(x = CANTIDAD_HORAS_TRABAJO, y = FALLO_VEHICULO)) +
  geom_point(aes(color = as.factor(FALLO_VEHICULO)), shape = "I", size = 3) + 
  geom_line(data = datos_curva, aes(y = probabilidad_FALLO_VEHICULO),
            color = "firebrick") + 
  geom_line(data = datos_curva, aes(y = limite_inferior_logit),
            linetype = "dashed") + 
  geom_line(data = datos_curva, aes(y = limite_superior_logit),
            linetype = "dashed") + 
  theme_bw() +
  labs(title = "Modelo regresión logística VAR_RES_11 ~ CANTIDAD_HORAS_TRABAJO",
       y = "P(FALLO_VEHICULO = 1 | CANTIDAD_HORAS_TRABAJO)", y = "CANTIDAD_HORAS_TRABAJO") + 
  theme(legend.position = "null") +
  theme(plot.title = element_text(hjust = 0.5))

#####################################################################################################
#####FIN

