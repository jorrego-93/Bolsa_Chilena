#BIBLIOTECAS
library(quantmod)#conect and import data from yahoo finance
library(PerformanceAnalytics)#optimizing portfolio
library(PortfolioAnalytics)#optimizing portfolio
library(forecast)#make predictions
library(tseries)#transform data to timeseries
library(urca)
library(ggplot2)#plotting
library(dplyr)#pipes and database operations
library(fTrading)#financial index
library(TTR)#financial index
library(xts)#transform data from ts to xts for financial index
library(corrplot)#make beutiful correlation matrix
library(rpart)#make desicion trees
library(rpart.plot)#desicion trees plots
library(caret)#machine learning package
library(e1071)
library(Metrics)#measuring model errors
library(rsample)#sampling data
library(purrr)
library(ipred)#ensemble trees
library(adabag)#ensemble trees
library(ROCR)#for area under the ROC curve
library(timeSeries)
library(fPortfolio)
library(randomForest)
library(caTools)
library(pROC)
library(ranger)
library(tictoc)
library(readxl)
library(usethis)

#IMPORTAR DATOS
empresas<- c("BCI.SN", "BSANTANDER.SN", "CCU.SN", "CENCOSUD.SN", "CHILE.SN", "CMPC.SN", "COLBUN.SN",
             "COPEC.SN", "ENELAM.SN", "FALABELLA.SN", "LTM.SN", "PARAUCO.SN",
             "SQM-B.SN", "ITAUCORP.SN", "ENTEL.SN", "ECL.SN")
dt.1<- "2013-01-01"
dt.2<- "2018-01-01"
LISTA<- list()
for (i in 1:length(empresas)) {
  LISTA[[i]]<- getSymbols.yahoo(empresas[i], from=dt.1,to=dt.2, auto.assign=F)
}
names(LISTA)<- empresas
#FIN IMPORT

#INDICADORES TECNICOS
indica<-c("MA.simple", "MA.conv.div", "MA.conv.div.signal", "stoch.fastK_14", "stoch.fastD_3", "stoch.SlowD_3", "P.roc", "P.Momentum", "WR", "rs.index", "mf.index") 
INDICADORES<- list()
for (name in empresas){
  INDICADORES[[name]]<- xts()
}
for (nombre in empresas){
  #Moving Average (15 periods, 3 semanas de actividad)
  INDICADORES[[nombre]]<- c(rep(NA, 14), SMA(x=LISTA[[nombre]][,4], n=15)) #14 NAs por n=15
  #Moving Average Cnvergeance Divergeance 
  INDICADORES[[nombre]]<- cbind(INDICADORES[[nombre]], MACD(x=LISTA[[nombre]][,4], nFast = 12, nSlow =26, percent = FALSE)[,1])
  INDICADORES[[nombre]]<- cbind(INDICADORES[[nombre]], MACD(x=LISTA[[nombre]][,4], nFast = 12, nSlow =26, percent = FALSE)[,2])
  #Stochastic Oscilator (K & D)
  INDICADORES[[nombre]]<- cbind(INDICADORES[[nombre]], stoch(HLC = cbind(LISTA[[nombre]][,2], LISTA[[nombre]][,3], LISTA[[nombre]][,4]),  nFastK = 14, nFastD = 3, nSlowD = 3)[,1])  #HLC : high-low-close matrix
  INDICADORES[[nombre]]<- cbind(INDICADORES[[nombre]], stoch(HLC = cbind(LISTA[[nombre]][,2], LISTA[[nombre]][,3], LISTA[[nombre]][,4]),  nFastK = 14, nFastD = 3, nSlowD = 3)[,2])
  INDICADORES[[nombre]]<- cbind(INDICADORES[[nombre]], stoch(HLC = cbind(LISTA[[nombre]][,2], LISTA[[nombre]][,3], LISTA[[nombre]][,4]),  nFastK = 14, nFastD = 3, nSlowD = 3)[,3])
  #Price Rate of Change ROC 
  INDICADORES[[nombre]]<- cbind(INDICADORES[[nombre]], ROC(x=LISTA[[nombre]][,4], n = 14))
  #Momentum (14)
  INDICADORES[[nombre]]<- cbind(INDICADORES[[nombre]], momentum(x=LISTA[[nombre]][,4], n=14))
  #Williams %R
  INDICADORES[[nombre]]<- cbind(INDICADORES[[nombre]], WPR(HLC = cbind(LISTA[[nombre]][,2], LISTA[[nombre]][,3], LISTA[[nombre]][,4]), n = 14))
  #Relative Strength Index RSI (14)
  INDICADORES[[nombre]]<- cbind(INDICADORES[[nombre]], RSI(price=LISTA[[nombre]][,4], n = 14))
  #Money Flow Index MFI (14)
  INDICADORES[[nombre]]<- cbind(INDICADORES[[nombre]], MFI(HLC = cbind(LISTA[[nombre]][,2], LISTA[[nombre]][,3], LISTA[[nombre]][,4]), volume = LISTA[[nombre]][,5], n = 14))
  names(INDICADORES[[nombre]])<- indica
}
names(INDICADORES[[1]])
#FIN INDICADORES


#LIMPIEZA
#LIMPIAR NULOS
nulos<-list()
for (name in empresas){
  nulos[[name]]<- matrix(ncol=length(indica))
  colnames(nulos[[name]])<- indica
}
#RUN NULOS
for (name in empresas){
  for (i in 1:length(indica)){
    nulos[[name]][,i]<- sum(is.na(INDICADORES[[name]][,i]))
  }
}
#TODOS LOS NULOS SON IGUALES para cada empresa

#VARIABLE SUBIDA DEL PRECIO, retorno y rendimiento. (del closing price)
nombre.columnas<- c(colnames(LISTA[[1]]), "Retorno.close", "Rendimiento.close.diario")
LISTA.completa<- LISTA
for ( j in empresas){
  LISTA.completa[[j]]<- cbind(LISTA.completa[[j]], (diff(LISTA[[j]][,4])), dailyReturn(LISTA[[j]][,4]))
  colnames(LISTA.completa[[j]])<- c(colnames(LISTA[[j]]), "Retorno.close", "Rendimiento.close.diario")
}
#VARIABLE PREDICTORA
var.pred<- list()
largo <-length(LISTA[[1]][,4])
for ( j in empresas){
  var.pred[[j]]<- as.vector(LISTA[[j]][-1, 4])-as.vector(LISTA[[j]][-largo, 4])
  var.pred[[j]]<- append(var.pred[[j]], NA)
}
names(var.pred)<- empresas

#CREATE DATA
colnames(LISTA.completa[[3]])
datos<- list()
for (name in empresas){
  datos[[name]]<- cbind(LISTA.completa[[name]][-1:-a.partir.d, ], INDICADORES[[name]][-1:-a.partir.d, ], var.pred[[name]][-1:-a.partir.d]>0 )
  colnames(datos[[name]])<- c(colnames(LISTA.completa[[name]]) , colnames(INDICADORES[[name]]) , "Prediccion")
}
names(datos)<- empresas
#MIS DATOS
LISTA.completa
INDICADORES
datos
class(datos[[1]][10,"Prediccion"])

head(datos[[1]], n=4)


