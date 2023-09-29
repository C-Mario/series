#Estudio Estadística descriptiva de las Series

#Librerias
library(TSstudio)
library(zoo)
library(xts)
library(readxl)
library(tidyverse)

#Cargar datos
load(file = "_environment.RData")

#Ajuste del formato de los datos
options(scipen = 12)
Exportaciones$Total <- round(Exportaciones$Total)
Exportaciones$Mes <- as.yearmon(Exportaciones$Mes)

# mirar fromato de exportaciones
str(Exportaciones)

# cambiar la ventana de datos
Exportaciones <- Exportaciones[97:378,]

data <- data.frame(as.Date(Exportaciones$Mes),Exportaciones$Total)

colnames(data) <- c("Fecha", "Dato")
str(data)

#Crear objeto de tipo ts indicandole la fecha de inicio y la frecuencia
data_ts <- ts(data$Dato,start = c(2000,01),frequency = 12)
TSstudio::ts_info(data_ts)
plot(data_ts, main = "Serie original")

###Creación objeto tssible a partir de un objeto tibble
library(feasts)
library(fable)
library(timetk)
library(tsibble)
library(lubridate)

df_exportaciones=data.frame(Valor=Exportaciones$Total,Fecha=Exportaciones$Mes)
tbl_exportaciones=tibble(df_exportaciones)
tbl_exportaciones_format_fecha=tbl_exportaciones
tbl_exportaciones_format_fecha$Fecha=yearmonth(tbl_exportaciones_format_fecha$Fecha)

###El tipo de fechas debe ser alguno que reconozca tsibble

tsbl_exportaciones=as_tsibble(tbl_exportaciones_format_fecha,index=Fecha)   ####La fecha en tsibble es importante

##Gráfica de la serie con tsibble
autoplot(tsbl_exportaciones,Valor)+labs(tittle="Serie de Exportaciones Mensuales en Colombia",y="Exportaciones en miles de dólares")
tbl_exportaciones$Fecha<-as.Date(zoo::as.yearmon(tbl_exportaciones$Fecha))
tbl_exportaciones

###Gráfica timetk
###Se presenta a continuación una gráfica de la serie de tiempo junto con una 
###estimación de la tendencia con el método de no paramétrico loess
tbl_exportaciones%>%plot_time_series(.value=Valor,.date_var=Fecha)


###Estabilización de la varianza 

#Función que entrega el valor de lambda para la transformación de Box Cox
#Aplicar sobre un obejeto de tipo ts
library(forecast)
forecast::BoxCox.lambda(data_ts, method ="loglik", lower = -1, upper = 3)
#El valor de lamba obtenido es 0.2 que no es muy cercano a 1 y tampoco es muy
#grande, por lo que se procede a hacer la transformación de Box Cox

#Ver como se transforma la serie para ese lambda
plot(forecast::BoxCox(data_ts,lambda=0.2))

#Se ve que la serie cambia un poco
#Se hace la transformación de la serie
ldata_ts=log(data_ts)

#Se comparan la serie original y la serie transformada
par(mar = c(1,1,1,1))
par(mfrow=c(2,1))
plot(data_ts,main="Serie de Exportaciones sin Transformar")
plot(ldata_ts,main="Series de Exportaciones con Transformación BoxCox")

#Se aprecia que ha cambiado la varianza marginal de la serie, pues se 
#ve que ahora tiene un comportamiento algo más constante

#Se verifica el valor de lambda para la serie transformada
forecast::BoxCox.lambda(ldata_ts, method ="guerrero", lower = -1, upper = 1)
#El valor de lambda para la tranformaciónd de Box Cox sobre la serire
#transformada es muy cercano a -1, cuando se espera que esté cercano a 1. Por lo que,
#no se tomará este valor de lambda para realizar la transformación

## Se puede transformar la serie usando Box-Cox de timetk
timetk::box_cox_vec(data_ts,lambda = 'auto',silent = F) #Usa lambda = -0.3342

#Otro método para verificar si hay que hacer una transformación que 
#estabilice la varianza

#Consiste en ver que valores de lambda maximizan la log-verosimilitud
#Si existe un lambda que la maximiza y ese lambda esta lejos de 1, 
#se sugiere realizar una tranformación de Box Cox

library(MASS)
MASS::boxcox(lm(data_ts ~ 1),seq(-1, 2, length = 50))

#A patir de la gráfica anterior que los valores que maximizan la log verosimilitud
#estan alrededor de 0.6, con un intervalo de congianza que no incluye al 1
#Se toma la decisión de transformar la serie


#La anterior tranformación se realizo con lambda igual a 0.2, se realizara esta vez con
#lambda=0.6 porque es el que maximiza la verosimilitus. Posteriormente se comparan 
#los resultados con lo obtenido anteriormente.

ldata_ts <- (1/0.6)*((data_ts^(0.6))-1)
plot(ldata_ts, main= "Serie transformada ("~lambda~"= 0.5 )")

#Se verifica ahora cual es el valor de lambda que maximiza la verosimilitud
#de la serie transformada
MASS::boxcox(lm(ldata_ts ~ 1),seq(0, 2, length = 50))
#Se realiza la comparación con la serie original
par(mar = c(1,1,1,1))
par(mfrow=c(2,1))
plot(data_ts,main="Serie de Exportaciones sin Transformar")
plot(ldata_ts,main="Series de Exportaciones con Transformación BoxCox")

#Se nota que no cambia mucho, al realizar la transformación con Lambda =0.5
#la serie se estabiliza más 

ldata_ts <- (1/0.5)*((data_ts^(0.5))-1)
plot(ldata_ts, main= "Serie transformada ("~lambda~"= 0.5 )")

#Veamos que valor de lambda maximiza la log verosimilitud de la serie transformada con
#lambda = 0.5. 

MASS::boxcox(lm(ldata_ts ~ 1),seq(0, 5, length = 50))
#Se obtienen valores de lambda que maximizan la 
#verosimilitud cercanos a uno. Por lo que se opta por realizar esta transformación
#Se realiza la comparación con la serie original
par(mar = c(1,1,1,1))
par(mfrow=c(2,1))
plot(data_ts,main="Serie de Exportaciones sin Transformar")
plot(ldata_ts,main="Series de Exportaciones con Transformación BoxCox")

#Otras opciones de transformación 

library(VGAM)
library(car)

prueba<-VGAM::yeo.johnson(data_ts, lambda = 0)
car::yjPower(AirPassengers,lambda=0)

############### Tendencia
## deterministica
## Se realiza un ajuste lineal de la serie explicada por el tiempo
summary(fit_data <- lm(ldata_ts~time(ldata_ts), na.action=NULL))
plot(ldata_ts, ylab="Datos en escala logar?tmica") 
abline(fit_data,col = "red") # Se añade la recta ajustada

###Eliminamos la tendencia con la predicción la recta
ElimiTendExport=ldata_ts-predict(fit_data)
plot(ElimiTendExport,main="Serie Exponencial Sin tendencia")
acf(ElimiTendExport,lag.max =282 )

## Descompocisión promedio movil
descompo_ldata <- decompose(ldata_ts)
plot(descompo_ldata)

###Eliminamos la tendencia con la predicción de la trend de promedios moviles
###Y se observa la serie resultante, para posteriomente realizar el análisis de
###estacionalidad
ElimiTendExport=ldata_ts-descompo_ldata$trend
plot(ElimiTendExport,main="Serie Exponencial Sin tendencia")
na.trim(ElimiTendExport)
acf(na.trim(ElimiTendExport),lag.max =282 )


#GRÁFICA AJUSTADA LOESS Y DESCOMPOCISIÓN STL con objeto TSSIBLLE
#
###Creación objeto tssible a partir de un objeto tibble
valores<-as.data.frame(ldata_ts)
df_exportaciones=data.frame(Valor=valores$x,Fecha=Exportaciones$Mes)
tbl_exportaciones=tibble(df_exportaciones)
tbl_exportaciones_format_fecha=tbl_exportaciones
tbl_exportaciones_format_fecha$Fecha=yearmonth(tbl_exportaciones_format_fecha$Fecha)

###El tipo de fechas debe ser alguno que reconozca tsibble

tsbl_exportaciones=as_tsibble(tbl_exportaciones_format_fecha,index=Fecha)   ####La fecha en tsibble es importante

##Gráfica de la serie con tsibble
autoplot(tsbl_exportaciones,Valor)+labs(tittle="Serie de Exportaciones Mensuales en Colombia",y="Exportaciones en miles de dólares")
tbl_exportaciones$Fecha<-as.Date(zoo::as.yearmon(tbl_exportaciones$Fecha))
tbl_exportaciones

###Gráfica timetk
###Se presenta a continuación una gráfica de la serie de tiempo junto con una 
###estimación de la tendencia con el método de no paramétrico loess
tbl_exportaciones%>%plot_time_series(.value=Valor,.date_var=Fecha)

## Descompocisión STL
str(tsbl_exportaciones)
tsbl_exportaciones %>%
  model(
    STL(Valor ~ trend() +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()

### DIFERENCIACIÓN

dx=diff(ldata_ts)
plot.ts(dx, main="Serie Diferenciada", ylab='')

###
acf(dx, 48, main = "Serie diferenciada")

#AUTOCORRELACIÓN
noTendldata_ts<- ElimiTendExport
#####AUTOCORRELACIÓN DE LOS REZAGOS
##### Se pueden observar 
#De la serie original
par(mar = c(3,2,3,2))
astsa::lag1.plot(data_ts, 12,corr=T)

#De la serie transformada con Box Cox
par(mar = c(3,2,3,2))
astsa::lag1.plot(ldata_ts, 12,corr=T)

#De la serie transformada con Box Cox y sin tendencia
par(mar = c(3,2,3,2))
astsa::lag1.plot(noTendldata_ts, 12,corr=T)

#De la serie diferenciada
par(mar = c(3,2,3,2))
astsa::lag1.plot(dx, 12,corr=T)

# AUTOCORRELACIÓN D ELA SERIE SIN TENDENCIA

noTendldata_ts<- ElimiTendExport

acf(na.trim(noTendldata_ts), lag.max = sqrt(length(noTendldata_ts)), main = "Serie transformada y sin tendencia")

### Grafico de autocorrelaciones con objeto tssibble
### 
tibble_soi_rec%>%plot_acf_diagnostics(Fecha_soi_rec,soi,.ccf_vars = rec,.lags = 36)



### ESTACIONALIDAD
### heatmap
noTendldata_ts<- ElimiTendExport
TSstudio::ts_heatmap(noTendldata_ts,title = "Mapa de Calor datos") 

#Comportamiento mensual de la serie sin tendencia
monthplot(noTendldata_ts)

## Comportamiento anual de la serie sin tendencia 
forecast::ggseasonplot(noTendldata_ts)






















######## periodograma
Periodograma <- spectrum(as.numeric(dldata),log='no')
#
ubicacionlogdata <- which.max(Periodograma$spec)
sprintf("El valor de la frecuencia donde se m?ximiza el periodograma para la serie es: %s",Periodograma$freq[ubicacionlogdata])
#
sprintf("El periodo correspondiente es aproximadamente: %s",1/Periodograma$freq[ubicacionlogdata])
#
perio <- astsa::mvspec(dldata, log="no")


##############################
##### estabilizaci?n de varianza
# estabilizar varianza
library(forecast)
forecast::BoxCox.lambda(data_ts, method = "loglik", lower = -5, upper = 5)
##Recordar que la funcion nos entrega el valor de lambda
library(MASS)
MASS::boxcox(lm(data_ts ~ 1),seq(-1, 2, length = 50))  
#####################################################
ldata_ts <- (1/0.2)*((data_ts^(0.2))-1)
plot(ldata_ts, main= "Serie transformada ("~lambda~"= 0.2 )")
### 
MASS::boxcox(lm(ldata_ts ~ 1),seq(0, 2, length = 50))
#
forecast::BoxCox.lambda(ldata_ts, method ="loglik", lower = -2, upper = 3)
###
acf(ldata_ts, main = "Serie transformada ("~lambda~"= 0.2 )")

















#Visualizar los datos
head(Data_xts)

#Información sobre la serie 
ts_info(Data_xts)

#Clase del objeto de serie de tiempo
class(Data_xts)

#Frecuencia de la serie
frequency(Data_xts)

#Periodo de la serie
periodicity(Data_xts)

#Clase del tiempo de la serie
tclass(Data_xts)

#Graficar la serie
plot(Data_xts)

ts_plot(Data_xts,
        title = "Exportaciones",
        Ytitle = "Exportaciones en dólares",
        Xtitle = "Mes",
        Xgrid = TRUE,
        Ygrid = TRUE)

#Se pueden ver algunas características d ela serie: Estacionalidad (por lo que hay pequeños picos)
#Posiblemente hay cuatro periodos de ciclos (del 2000 al 2009, del 2009 al 2016, del 2016 al 2020 y 
#del 2020 al 2023). Se puede apreciar un poco de heterosedasticidad 



#Análisis de Tendencias

Un primer intento de ver como sería la tendencia sería a través del uso
del filtro de promedios móviles.

## Descomposición por Filtro de Promedios Móviles

export_decompo=decompose(Data_xts)
plot(export_decompo)
export_decompo$trend
