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
library(Rmisc)
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


#VARIABLES
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
#LISTA.completa
#INDICADORES
#datos
#class(datos[[1]][10,"Prediccion"])

#head(datos[[1]], n=4)

#Classificacion
datos.CLASS<- list()
for (name in empresas){
  datos.CLASS[[name]]<- datos[[name]][, c(9:20)]
  datos.CLASS[[name]]<- datos.CLASS[[name]][-length(datos.CLASS[[name]][,1]),] #Boto la ultima fila porque no conozco la Prediccion
  colnames(datos.CLASS[[name]])<- colnames(datos[[1]][,9:20])
}

#THRESHOLD FUNCTIONS
threshold_finder<- function(predicciones, referencia){ #referencia = validation data
  threshold<- seq(0.05, 0.95, by = 0.005)
  TN.FN_optimo<- 0
  threshold_optimo<- 0
  for (i in 1:length(threshold)){
    pred<- ifelse(predicciones[,1]>threshold[i], 0, 1)
    if (sum(pred==1)!=length(pred) & sum(pred==1)>4){ #que no sean solo subidas o bajadas
      aers<-confusionMatrix(table(data=as.vector(pred), 
                                  reference=as.vector(referencia)))
      TN.FN_ratio<-((0.1+aers$table[2,2])/(0.1+aers$table[2,1]+aers$table[2,2]))
      threshold_optimo<- ifelse(TN.FN_ratio>TN.FN_optimo, threshold[i], threshold_optimo)
      TN.FN_optimo<- ifelse(TN.FN_ratio>TN.FN_optimo, TN.FN_ratio, TN.FN_optimo)} else{
      }
  }
  return (list(corte=threshold_optimo, True.neg_False.pos=TN.FN_optimo))
}
thresh_optimizer<- function (modelo, data){
  n_thresh<- 2
  contador<- 100
  thresh_promedio<- c()
  while (contador>0.2){
    assignment2<- sample(1:n_thresh, size=nrow(data), 
                         prob=c(rep(1/n_thresh, n_thresh)), replace=TRUE)
    muestras<- list()
    predicciones<- list()
    thresh_try<- list()
    for (i in 1:n_thresh){
      muestras[[i]]<- data[assignment2==i, ]
    }
    for (i in 1:n_thresh){
      predicciones[[i]]<- predict(modelo, muestras[[i]], type="response")$predictions #CAMBIAR AQUI (para RF: prob a response y agregar $predictions; para el resto: lo contrario)
      thresh_try[[i]]<- threshold_finder(predicciones = predicciones[[i]], 
                                         referencia = muestras[[i]]$Prediccion)$corte
    }
    thresh_promedio<- cbind(thresh_promedio, rowMeans(as.data.frame(thresh_try)))
    if (n_thresh>3){
      contador<- abs(thresh_promedio[length(thresh_promedio)]-thresh_promedio[length(thresh_promedio)-1])/thresh_promedio[length(thresh_promedio)-1]
      n_thresh<- n_thresh*2
    }else {
      n_thresh<- n_thresh*2
    }
  }
  return (list(corte= mean(c(thresh_promedio[length(thresh_promedio)], thresh_promedio[length(thresh_promedio)-1])), thresholds=thresh_promedio, n_iteraciones=n_thresh, error=contador))
}


#MODELOS PREDICTORES SUBIDA Y BAJADA DE PRECIO
#------------------------------------------------------------------
#RANDOM FOREST
Compilado.rf<- NULL
modelos.rf<- NULL
mejor.rf<-NULL
mejor.rf1<- NULL


#RANDOM FOREST
set.seed(1234)
modelos.rf<- list()
thresholds.rf<- list()
resultados.rf<- list()
Compilado.rf<- list()
tic()
for (mm in 1:16) {
  m<-mm
  #  set.seed(1234)
  assignment1<- sample(1:3, size=nrow(datos.CLASS[[m]]), prob=c(0.6,0.30,0.10), replace=TRUE)
  tree.train1<-datos.CLASS[[m]][assignment1==1, ]
  tree.valid1<-datos.CLASS[[m]][assignment1==2, ]
  tree.test1<-datos.CLASS[[m]][assignment1==3, ]
  tree.train_factor1<- as.data.frame(tree.train1)
  tree.valid_factor1<- as.data.frame(tree.valid1)
  tree.test_factor1<- as.data.frame(tree.test1)
  tree.train_factor1$Prediccion <- factor(tree.train_factor1$Prediccion)
  tree.valid_factor1$Prediccion <- factor(tree.valid_factor1$Prediccion)
  tree.test_factor1$Prediccion <- factor(tree.test_factor1$Prediccion) 
  
  #RANDOM FOREST MODEL
  #Hyper-grid search
  mtry2<- seq(2, ncol(tree.train1)*0.8,1)
  nodesize2<- seq(2,12,2)
  sampsize2<- seq(0.2,0.8,0.2) #usar 0.05
  maxdepth2<- seq(0,30,10) #usar 10  poner de 10 a 50
  
  hyper_grid.rf2<- expand.grid(mtry=mtry2, nodesize=nodesize2, 
                               sampsize=sampsize2, max.depth=maxdepth2)
  oob_err2<- c()
  
  for(i in 1:nrow(hyper_grid.rf2)){
    model2<- ranger(formula=Prediccion~. , data= tree.train_factor1,
                    num.trees = 100, write.forest = FALSE, probability = TRUE,
                    classification = TRUE, oob.error = TRUE, seed=1234, 
                    mtry=hyper_grid.rf2$mtry[i],
                    min.node.size=hyper_grid.rf2$nodesize[i],
                    sample.fraction = hyper_grid.rf2$sampsize[i],
                    max.depth = hyper_grid.rf2$max.depth[i])
    oob_err2[i]<- model2$prediction.error
  }
  #SELECT MIN Out Of Bag (OOB) Error MODEL
  #  oob_err2
  #  oob_err2[which.min(oob_err2)]
  opt_i2<- which.min(oob_err2)
  exitoo2<-(hyper_grid.rf2[opt_i2,])
  #  exitoo2
  
  #MODELO RANDOM FOREST OPTIMO
  mejor.rf1<- ranger(formula=Prediccion~. , data= tree.train_factor1,
                     num.trees = 100, write.forest = TRUE, probability = TRUE,
                     classification = TRUE, oob.error = TRUE, seed=1234, 
                     mtry=exitoo2$mtry,
                     min.node.size=exitoo2$nodesize,
                     sample.fraction = exitoo2$sampsize,
                     max.depth = exitoo2$max.depth)
  
  #THRESHOLD FINDER RF
  threshold.rf<-thresh_optimizer(modelo=mejor.rf1, data=tree.train1)
  print(paste0("Corte Modelo #", m, " : ", threshold.rf$corte))
  rf.test<- predict(mejor.rf1, tree.test1, type="response")
  if (sum(ifelse(rf.test$predictions[,1]>threshold.rf$corte, 0, 1))==0) {
    resultados.optimos.rf<- "No hay subidas predecidas"
    print(paste0("Modelo #", m, " : ", "No hay subidas predecidas"))
  }else{
    resultados.optimos.rf<- Resultados_Optimos(corte = threshold.rf$corte , predicciones = rf.test$predictions, referencia = tree.test1$Prediccion)
  }
  modelos.rf[[m]]<- mejor.rf1
  thresholds.rf[[m]]<- threshold.rf
  resultados.rf[[m]]<- resultados.optimos.rf
  Compilado.rf[[m]]<- list(modelo= modelos.rf[[m]], threshold = thresholds.rf[[m]], resultados = resultados.rf[[m]])
}
toc()

#TABLA RESULTADOS Random Forest
tabla.resultados.rf<- data.frame()
for (i in 1:length(modelos.rf)) {
  tabla.resultados.rf[i,1]<-  round(modelos.rf[[i]]$prediction.error, digits = 3)
  tabla.resultados.rf[i,2]<-modelos.rf[[i]]$min.node.size
  tabla.resultados.rf[i,3]<-modelos.rf[[i]]$mtry
  tabla.resultados.rf[i,4]<-round(100*thresholds.rf[[i]]$corte, digits=3)
  tabla.resultados.rf[i,5]<-thresholds.rf[[i]]$n_iteraciones
  if(resultados.rf[[i]]== "No hay subidas predecidas"){
    tabla.resultados.rf[i,6]<- "No subidas"
    tabla.resultados.rf[i,7]<- "No subidas"
    tabla.resultados.rf[i,8]<- "No subidas"
  }else{
    tabla.resultados.rf[i,6]<-resultados.rf[[i]]$table[2,2]
    tabla.resultados.rf[i,7]<-resultados.rf[[i]]$table[2,1]
    tabla.resultados.rf[i,8]<-round(resultados.rf[[i]]$table[2,2]/(resultados.rf[[i]]$table[2,1]+resultados.rf[[i]]$table[2,2]), digits = 3)
  }}
colnames(tabla.resultados.rf)<- c("Error", "Min Node Size", "mtry", "Threshold", "# Iteraciones", "T.Neg", "F.Neg", "Ratio TN/FN")
tabla.resultados.rf
mean(as.double(tabla.resultados.rf[-c(9,11,13,15,16), 8]))*100




#------------------------------------------------------------------------------------
#BAGGED TREES
set.seed(1234)
modelos.bagged.tree<- list()
thresholds.bagged.tree<- list()
resultados.bagged.tree<- list()
Compilado.bagged.tree<- list()
tic()
for (i in 1:16) {
  m<-i
  assignment2<- sample(1:3, size=nrow(datos.CLASS[[m]]), prob=c(0.60,0.30,0.10), replace=TRUE)
  tree.train2<-datos.CLASS[[m]][assignment2==1, ]
  tree.valid2<-datos.CLASS[[m]][assignment2==2, ]
  tree.test2<-datos.CLASS[[m]][assignment2==3, ]
  tree.train_factor2<- as.data.frame(tree.train2)
  tree.valid_factor2<- as.data.frame(tree.valid2)
  tree.test_factor2<- as.data.frame(tree.test2)
  tree.train_factor2$Prediccion <- factor(tree.train_factor2$Prediccion)
  tree.valid_factor2$Prediccion <- factor(tree.valid_factor2$Prediccion)
  tree.test_factor2$Prediccion <- factor(tree.test_factor2$Prediccion)
  
  ctrl<- trainControl(method="cv",number= 10,
                      classProbs = TRUE, 
                      summaryFunction = twoClassSummary,
                      verboseIter=TRUE)
  modelo.bagged<- train(Prediccion~. ,data= tree.train_factor2, type="class",
                        method="treebag", control=(minsplit=0),
                        metric="Accuracy", trainControl=ctrl, nbagg=100)
  p13<- predict(modelo.bagged, newdata = tree.valid_factor2, type="prob")
  #p13.thresh<- threshold_finder(predicciones = p13, referencia = tree.valid_factor2$Prediccion)
  #p13.thresh
  threshold.bagged<-thresh_optimizer(modelo=modelo.bagged, data=tree.valid_factor2)
  #threshold.bagged$corte
  p13.test<- predict(modelo.bagged, tree.test_factor2, type="prob")
  resultados.optimos.bagged<-Resultados_Optimos(corte=threshold.bagged$corte, predicciones=p13.test, referencia=tree.test_factor2$Prediccion)
  modelos.bagged.tree[[m]]<- modelo.bagged
  thresholds.bagged.tree[[m]]<- threshold.bagged
  resultados.bagged.tree[[m]]<- resultados.optimos.bagged
  Compilado.bagged.tree[[m]]<- list(modelo= modelos.bagged.tree[[m]], threshold = thresholds.bagged.tree[[m]], resultados = resultados.bagged.tree[[m]])
  #confusionMatrix(table(data=ifelse(p13.test[,1]>threshold.bagged$corte, 0, 1), reference=tree.test_factor2$Prediccion)) 
}
toc()
length(Compilado.bagged.tree)
length(modelos.bagged.tree)

#Tabla Resultados Bagged
tabla.resultados.bagged<- data.frame()
for (i in 1:length(modelos.bagged.tree)) {
  tabla.resultados.bagged[i,1]<-  round(modelos.bagged.tree[[i]]$results[, "Accuracy"], digits = 3)
  tabla.resultados.bagged[i,2]<-round(100*thresholds.bagged.tree[[i]]$corte, digits=3)
  tabla.resultados.bagged[i,3]<-thresholds.bagged.tree[[i]]$n_iteraciones
  if(resultados.bagged.tree[[i]]== "No hay subidas predecidas"){
    tabla.resultados.bagged[i,4]<- "No subidas"
    tabla.resultados.bagged[i,5]<- "No subidas"
    tabla.resultados.bagged[i,6]<- "No subidas"
  }else{
    tabla.resultados.bagged[i,4]<-resultados.bagged.tree[[i]]$table[2,2]
    tabla.resultados.bagged[i,5]<-resultados.bagged.tree[[i]]$table[2,1]
    tabla.resultados.bagged[i,6]<-round(resultados.bagged.tree[[i]]$table[2,2]/(resultados.bagged.tree[[i]]$table[2,1]+resultados.bagged.tree[[i]]$table[2,2]), digits = 3)
  }}
colnames(tabla.resultados.bagged)<- c("Accuracy", "Threshold", "# Iteraciones", "T.Neg", "F.Neg", "Ratio TN/FN")
tabla.resultados.bagged
mean(as.double(tabla.resultados.bagged[, 6]))*100




#----------------------------------------------------------------------------
#DECISION TREES
#HYPER-GRID
modelos.hyper.tree<- list()
thresholds.hyper.tree<- list()
resultados.hyper.tree<- list()
Compilado.hyper.tree<- list()
tic()
for (mm in 1:16) {
  set.seed(1234)
  m<-mm
  assignment2<- sample(1:3, size=nrow(datos.CLASS[[m]]), prob=c(0.60,0.30,0.10), replace=TRUE)
  tree.train2<-datos.CLASS[[m]][assignment2==1, ]
  tree.valid2<-datos.CLASS[[m]][assignment2==2, ]
  tree.test2<-datos.CLASS[[m]][assignment2==3, ]
  tree.train_factor2<- as.data.frame(tree.train2)
  tree.valid_factor2<- as.data.frame(tree.valid2)
  tree.test_factor2<- as.data.frame(tree.test2)
  tree.train_factor2$Prediccion <- factor(tree.train_factor2$Prediccion)
  tree.valid_factor2$Prediccion <- factor(tree.valid_factor2$Prediccion)
  tree.test_factor2$Prediccion <- factor(tree.test_factor2$Prediccion)
  
  splits.tree<- seq(5, 30, 5) #especificar minsplit o minbucket ya que minsplit=minbucket*3. 
  depth.tree<- seq(5, 30, 1) #maximo es 30 por algoritmo
  surrogate.tree<- c(0,1)
  split.tree<- c("gini", "information")
  hyper_grid.tree<- expand.grid(minsplit=splits.tree,
                                maxdepth=depth.tree, surrogatestyle=surrogate.tree,
                                splits.tree=split.tree<- c("gini", "information"))
  modelos.tree<- list()
  rmse_values.tree<- c()
  for (i in 1:nrow(hyper_grid.tree)){
    minsplit<- hyper_grid.tree$minsplit[i]
    maxdepth<- hyper_grid.tree$maxdepth[i]
    surrogatestyle= hyper_grid.tree$surrogatestyle[i]
    split.criteria=splits.tree[i]
    modelos.tree[[i]]<-  rpart(Prediccion ~ . , data= tree.train2,
                               method="class", parms=list(split=split.criteria),
                               minsplit=minsplit, maxdepth=maxdepth, 
                               surrogatestyle = surrogatestyle,
                               xval=10)
  }
  for (i in 1:length(modelos.tree)){
    model <- modelos.tree[[i]]
    pred<- predict(object=model, newdata=tree.valid2)
    rmse_values.tree[i]<- rmse(actual=as.integer(tree.valid2$Prediccion), predicted=as.integer(pred))
  }
  #elegir mejor modelo ( y ver hyper grid usado)
  mejor_modelo<- modelos.tree[[which.min(rmse_values.tree)]]
  #  hyper_grid.tree[which.min(rmse_values.tree), ]
  #Elegir cp optimo
  #Visualizacion CP (error vs tree size)
  plotcp(mejor_modelo)
  opt_index.mejor_modelo<- which.min(mejor_modelo$cptable[, "xerror"])
  cp_optimo.mejor_modelo<- mejor_modelo$cptable[opt_index.mejor_modelo, "CP"]
  if ( which.min(mejor_modelo$cptable[, "xerror"]) ==1){
    mejor_modelo_opt_cp<- mejor_modelo}else{
      mejor_modelo_opt_cp<- prune(tree=mejor_modelo, cp=cp_optimo.mejor_modelo)
    }
  #OPTIMAL THRESHOLD
  #  mejor_modelo_opt_cp<- mejor_modelo
  threshold.hyper.tree<-thresh_optimizer(modelo=mejor_modelo_opt_cp, data=tree.train2)
  
  #  threshold.tree$thresholds
  #  threshold.tree$corte
  tree.model.test<- predict(mejor_modelo_opt_cp, tree.test2, type="prob")
  resultados.optimos.tree<- Resultados_Optimos(corte = threshold.hyper.tree$corte , predicciones = tree.model.test, referencia = tree.test2$Prediccion)
  modelos.hyper.tree[[m]]<- mejor_modelo_opt_cp
  thresholds.hyper.tree[[m]]<- threshold.hyper.tree
  resultados.hyper.tree[[m]]<- resultados.optimos.tree
  Compilado.hyper.tree[[m]]<- list(modelo= modelos.hyper.tree[[m]], threshold = thresholds.hyper.tree[[m]], resultados = resultados.hyper.tree[[m]])
  #confusionMatrix(table(data=ifelse(p13.test[,1]>threshold.bagged$corte, 0, 1), reference=tree.test_factor2$Prediccion)) 
}
toc()

Compilado.hyper.tree[[14]]$threshold$n_iteraciones
resultados.hyper.tree[[16]]

Compilado.autoprune.tree[[1]]$resultados

#TABLA RESULTADOS ARBOL DECISION (Hyper Grid)
tabla.resultados.htree<- data.frame()
for (i in 1:length(modelos.hyper.tree)) {
  tabla.resultados.htree[i,1]<-  modelos.hyper.tree[[i]]$cptable[which.min(modelos.hyper.tree[[i]]$cptable[, "xerror"]), 1]
  tabla.resultados.htree[i,2]<-  round(modelos.hyper.tree[[i]]$cptable[which.min(modelos.hyper.tree[[i]]$cptable[, "xerror"]), "xerror"], digits = 4)
  tabla.resultados.htree[i,3]<-modelos.hyper.tree[[i]]$control$minsplit
  tabla.resultados.htree[i,4]<-modelos.hyper.tree[[i]]$control$maxdepth
  tabla.resultados.htree[i,5]<-round(100*thresholds.hyper.tree[[i]]$corte, digits=3)
  tabla.resultados.htree[i,6]<-thresholds.hyper.tree[[i]]$n_iteraciones
  if(resultados.hyper.tree[[i]]== "No hay subidas predecidas"){
    tabla.resultados.htree[i,7]<- "No subidas"
    tabla.resultados.htree[i,8]<- "No subidas"
    tabla.resultados.htree[i,9]<- "No subidas"
  }else{
    tabla.resultados.htree[i,7]<-resultados.hyper.tree[[i]]$table[2,2]
    tabla.resultados.htree[i,8]<-resultados.hyper.tree[[i]]$table[2,1]
    tabla.resultados.htree[i,9]<-round(resultados.hyper.tree[[i]]$table[2,2]/(resultados.hyper.tree[[i]]$table[2,1]+resultados.hyper.tree[[i]]$table[2,2]), digits = 3)
  }}
colnames(tabla.resultados.htree)<- c("CP", "xError CP", "Min Split","Max Depth", "Threshold", "# Iteraciones", "T.Neg", "F.Neg", "Ratio TN/TN+FN")
tabla.resultados.htree
mean(as.double(tabla.resultados.htree[, 9]))*100



#PERIODO DE PRUEBA, ROLLING WINDOW ANO 2017
#------------------------------------------------------------------------
periodo_2017<- 244
espacio_tiempo<- 20  #DIAS
rango_tiempo<- seq(from=1, to=225, by=1 )
tablas.rendimientos<- list()
tabla.ocio <- matrix(0, nrow=periodo_2017-espacio_tiempo+1, ncol=3)
cuenta.lugares<- 1
#TEST SAMPLE
#tail(datos.CLASS[[1]],n=246)

#225+espacio_tiempo-1
for (l in rango_tiempo){
  pf.test.sample<- list()
  pf.test_factor<- list()
  
  for (mm in 1:16) {
    set.seed(1234)
    m<-mm
    largo.test<- nrow(datos.CLASS.final[[m]])
    pf.test.sample[[m]]<-tail(datos.CLASS.final[[m]], n=periodo_2017)[l:(l+espacio_tiempo-1),]
    pf.test_factor[[m]]<- as.data.frame(pf.test.sample[[m]])
    pf.test_factor[[m]]$Prediccion <- factor(pf.test_factor[[m]]$Prediccion)
  }
  
  #MATRIZ RETORNOS IPSA EQUALY WEIGHTED (corrido un dia xq retorno se tiene que multiplicar por pred subida dia anterior)
  retornos_prueba<- matrix(0, nrow=nrow(pf.test.sample[[1]]), ncol=length(datos.CLASS))
  for (mm in 1:16) {
    retornos_prueba[,mm]<- tail(LISTA.completa.final[[mm]], n=periodo_2017)[l:(l+espacio_tiempo-1),]$Rendimiento.close.diario
  }
  matriz.pesos.general<- matrix(1/16, nrow=nrow(pf.test.sample[[1]]), ncol=length(datos.CLASS))
  #sum(colSums(retornos_prueba*matriz.pesos.general))
  
  
  #MATRIZ PESOS RETORNOS IPSA WEIGHTED (corrido un dia xq retorno se tiene que multiplicar por pred subida dia anterior)
  #sum(IPSA)
  matriz.pesos.IPSA<- matrix(IPSA, nrow=nrow(pf.test.sample[[1]]), ncol=length(datos.CLASS), byrow=TRUE)
  #sum(colSums(retornos_prueba*matriz.pesos.IPSA))
  
  
  #RANDOM FOREST
  #tabla.resultados.rf
  modelos.utiles.rf<- c(1,2,3,4,5,8,9,10,11,12,13,14,15)
  #MATRIZ PREDICCION RANDOM FOREST
  prediccion_randomForest<- matrix(0, nrow=nrow(pf.test.sample[[1]]), ncol=length(datos.CLASS))
  for (mm in modelos.utiles.rf) {
    set.seed(1234)
    m<-mm
    prueba_randomForest<- predict(modelos.rf[[m]], pf.test.sample[[m]], type="response")
    prediccion_randomForest[,m]<- ifelse(prueba_randomForest$predictions[,1]>thresholds.rf[[m]]$corte, 0, 1)
  }
  head(prediccion_randomForest)
  #dias ociosos
  ocio_rf<- sum(ifelse(rowSums(prediccion_randomForest)==0,1,0))
  
  #MATRIZ PESOS PORTFOLIO EQUALY WEIGHTED
  matriz.pesos.rf<- matrix(0, nrow=nrow(pf.test.sample[[1]]), ncol=length(datos.CLASS))
  for (mm in 1:nrow(prediccion_randomForest)) {
    if (sum(prediccion_randomForest[mm,]>0)){
      matriz.pesos.rf[mm, ]<- prediccion_randomForest[mm,]/sum(prediccion_randomForest[mm,])
    }else{
      matriz.pesos.rf[mm, ]<- prediccion_randomForest[mm,]
    }
  }
  #head(matriz.pesos.rf)
  #matriz.pesos
  #MATRIZ PORTFOLIO EQUALY WEIGHTED RANDOM FOREST
  portfolio.randomForest_equaly<- prediccion_randomForest*retornos_prueba*matriz.pesos.rf
  colnames(portfolio.randomForest_equaly)<- empresas
  #colSums(portfolio.randomForest_equaly)
  #sum(colSums(portfolio.randomForest_equaly))
  
  #COMPARACION PORCENTUAL
  #sum(colSums(portfolio.randomForest_equaly))/sum(colSums(retornos_prueba*matriz.pesos.general))*100
  
  
  
  #BAGGED TREES
  
  #tabla.resultados.bagged
  modelos.utiles.bagged.trees<- c(which(tabla.resultados.bagged$`Ratio TN/FN`>0.5))
  #MATRIZ PREDICCION BAGGED TREES
  prediccion_baggedTrees<- matrix(0, nrow=nrow(pf.test_factor[[1]]), ncol=length(datos.CLASS))
  for (mm in modelos.utiles.bagged.trees) {
    set.seed(1234)
    m<-mm
    prueba_baggedTrees<- predict(modelos.bagged.tree[[m]], pf.test_factor[[m]], type="prob")
    prediccion_baggedTrees[,m]<- ifelse(prueba_baggedTrees[,1]>thresholds.bagged.tree[[m]]$corte, 0, 1)
  }
  #head(prediccion_baggedTrees)
  #dias ociosos
  ocio_baggedTrees<- sum(ifelse(rowSums(prediccion_baggedTrees)==0,1,0))
  
  #MATRIZ PESOS PORTFOLIO EQUALY WEIGHTED BAGGED TREES
  matriz.pesos.baggedTrees<- matrix(0, nrow=nrow(pf.test_factor[[1]]), ncol=length(datos.CLASS))
  for (mm in 1:nrow(prediccion_baggedTrees)) {
    if (sum(prediccion_baggedTrees[mm,]>0)){
      matriz.pesos.baggedTrees[mm, ]<- prediccion_baggedTrees[mm,]/sum(prediccion_baggedTrees[mm,])
    }else{
      matriz.pesos.baggedTrees[mm, ]<- prediccion_baggedTrees[mm,]
    }
  }
  #head(matriz.pesos.baggedTrees)
  
  #MATRIZ PORTFOLIO EQUALY WEIGHTED BAGGED TREES
  portfolio.baggedTrees_equaly<- prediccion_baggedTrees*retornos_prueba*matriz.pesos.baggedTrees
  colnames(portfolio.baggedTrees_equaly)<- empresas
  #colSums(portfolio.baggedTrees_equaly)
  #sum(colSums(portfolio.baggedTrees_equaly))
  
  #COMPARACION PORCENTUAL BAGGED TREES
  #sum(colSums(portfolio.baggedTrees_equaly))/sum(colSums(retornos_prueba*matriz.pesos.general))*100
  
  
  
  #DECISION TREES
  
  #tabla.resultados.htree
  modelos.utiles.htree<- c(which(tabla.resultados.htree$`Ratio TN/TN+FN`>0.5))
  #MATRIZ PREDICCION DECISION TREES
  prediccion_htree<- matrix(0, nrow=nrow(pf.test_factor[[1]]), ncol=length(datos.CLASS))
  for (mm in modelos.utiles.htree) {
    set.seed(1234)
    m<-mm
    prueba_htree<- predict(modelos.hyper.tree[[m]], pf.test_factor[[m]], type="prob")
    prediccion_htree[,m]<- ifelse(prueba_htree[,1]>thresholds.hyper.tree[[m]]$corte, 0, 1)
  }
  #head(prediccion_htree)
  #sum(colSums(prediccion_htree))
  #dias ociosos
  ocio_htree<- sum(ifelse(rowSums(prediccion_htree)==0,1,0))
  
  #MATRIZ PESOS PORTFOLIO EQUALY WEIGHTED DECISION TREES
  matriz.pesos.htree<- matrix(0, nrow=nrow(pf.test_factor[[1]]), ncol=length(datos.CLASS))
  for (mm in 1:nrow(prediccion_htree)) {
    if (sum(prediccion_htree[mm,]>0)){
      matriz.pesos.htree[mm, ]<- prediccion_htree[mm,]/sum(prediccion_htree[mm,])
    }else{
      matriz.pesos.htree[mm, ]<- prediccion_htree[mm,]
    }
  }
  #head(matriz.pesos.htree)
  
  #MATRIZ PORTFOLIO EQUALY WEIGHTED DECISION TREES
  portfolio.htree_equaly<- prediccion_htree*retornos_prueba*matriz.pesos.htree
  colnames(portfolio.htree_equaly)<- empresas
  #colSums(portfolio.htree_equaly)
  #sum(colSums(portfolio.htree_equaly))
  
  #COMPARACION PORCENTUAL DECISION TREES
  #sum(colSums(portfolio.htree_equaly))/sum(colSums(retornos_prueba*matriz.pesos.general))*100
  
  
  
  
  #RETORNOS IPSA
  SP_IPSA_SN <- read_excel("C:\Users\jampr\OneDrive\Escritorio\Memoria\Variables\variables_yahoo_finance\SP-IPSA.SN-2018.xls")
  SP_IPSA_SN<- na.omit(SP_IPSA_SN)
  colnames(SP_IPSA_SN)<- c("Periodo", "Precio")
  #tail(LISTA.completa[[1]]$Retorno.close)
  IPSA_retornos<- cbind(SP_IPSA_SN[-1,1], (SP_IPSA_SN[-1,2]-SP_IPSA_SN[-nrow(SP_IPSA_SN),2])/SP_IPSA_SN[-1,2] )
  ipsa_30dias<- sum(tail(IPSA_retornos, n=periodo_2017)[l:(l+espacio_tiempo-1),2])
  
  portfolio.htree_equaly
  #COMPARACIONES
  
  rendimientos.1<-c(sum(colSums(portfolio.htree_equaly)), sum(colSums(portfolio.baggedTrees_equaly)), 
                    sum(colSums(portfolio.randomForest_equaly)), sum(colSums(retornos_prueba*matriz.pesos.general)),ipsa_30dias)
  rendimientos.2<-rendimientos.1/rendimientos.1[4]
  rendimientos.3<-rendimientos.1/rendimientos.1[5]
  rendimientos.4<-c(sum(colSums(prediccion_htree)), sum(colSums(prediccion_baggedTrees)), 
                    sum(colSums(prediccion_randomForest)), sum(colSums(retornos_prueba*matriz.pesos.general)),ipsa_30dias)
  aciertos.comparacion<-c(sum(colSums(portfolio.htree_equaly>0)), sum(colSums(portfolio.baggedTrees_equaly>0)), 
                          sum(colSums(portfolio.randomForest_equaly>0)), sum(colSums(retornos_prueba*matriz.pesos.general>0)),sum(tail(IPSA_retornos, n=periodo_2017)[l:(l+espacio_tiempo-1),2]>0))
  equivocaciones.comparacion<- c(sum(colSums(portfolio.htree_equaly<0)), sum(colSums(portfolio.baggedTrees_equaly<0)), 
                                 sum(colSums(portfolio.randomForest_equaly<0)), sum(colSums(retornos_prueba*matriz.pesos.general<0)),sum(tail(IPSA_retornos, n=periodo_2017)[l:(l+espacio_tiempo-1),2]<0))
  rendimientos.5<- aciertos.comparacion/(aciertos.comparacion+equivocaciones.comparacion)
  
  rendimientos<- matrix(c(rendimientos.1, rendimientos.2, rendimientos.3, rendimientos.4, rendimientos.5), nrow=5, ncol=5, byrow = TRUE)
  colnames(rendimientos)<- c("HyperTree", "BaggedTrees", "RandomForest","Equally Weighted 16" ,"IPSA")
  rownames(rendimientos)<- c("Rend. Mensual", "Rend. respecto EW16" ,"Rend. respecto IPSA", "Turnover", "Hit Ratio")
  tablas.rendimientos[[cuenta.lugares]]<- rendimientos
  tabla.ocio[cuenta.lugares,]<- c(ocio_htree,ocio_baggedTrees,ocio_rf)
  cuenta.lugares<- cuenta.lugares+1
}



#RESULTADOS ROLLING WINDOW
#--------------------------------------------------------------------------------------

dataset.resultados<- as.data.frame(cbind(retornos.htrees,retornos.baggedTrees, retornos.randomForest, retornos.EW16,retornos.IPSA))
colnames(dataset.resultados)<- c("Dec.TreeHyper", "BaggedTrees", "RandomForest", "EW 16", "IPSA")
boxplot(dataset.resultados, las=2, col= NULL,cex.axis=0.7)


mostrando.rendimientos<- matrix(c(100*sapply(dataset.resultados, mean), 100*sapply(dataset.resultados, var), sapply(dataset.resultados, skewness), sapply(dataset.resultados, kurtosis)), nrow=4, ncol=5, byrow=TRUE)
colnames(mostrando.rendimientos)<- c("Dec.TreeHyper", "BaggedTrees", "RandomForest", "EW 16", "IPSA")
rownames(mostrando.rendimientos)<- c("Media 100%", "Varianza 100%", "Asimetr?a", "Curtosis")
mostrando.rendimientos


sharpe.tanteo<- mostrando.rendimientos[1,]/sqrt(mostrando.rendimientos[2,])
sharpe.tanteo


tracking.error<- sapply(dataset.resultados[,1:3]-dataset.resultados[,5], sd)
tracking.error
information.ratio<- (mostrando.rendimientos[1,1:3]-mostrando.rendimientos[1,5])/tracking.error
information.ratio
head(dataset.resultados)

(row.names(tabla.resultados.htree)<- empresas)
(row.names(tabla.resultados.bagged)<- empresas)
(row.names(tabla.resultados.rf)<- empresas)
tabla.resultados.htree
tabla.resultados.bagged
tabla.resultados.rf

hist(retornos.htrees)
hist(retornos.baggedTrees)
hist(retornos.randomForest)
hist(retornos.EW16)
hist(retornos.IPSA)


prediccion_htree
prediccion_baggedTrees
prediccion_randomForest
colSums(portfolio.htree_equaly)
colSums(portfolio.baggedTrees_equaly)
colSums(portfolio.randomForest_equaly)


portfolio.htree_equaly
portfolio.baggedTrees_equaly
portfolio.randomForest_equaly


hitratio.htrees<- c()
hitratio.baggedTrees<- c()
hitratio.randomForest<- c()
hitratio.EW16<- c()
hitratio.IPSA<-c()
for (i in 1:length(tablas.rendimientos)){
  hitratio.htrees[i]<- tablas.rendimientos[[i]][5,1]
  hitratio.baggedTrees[i]<- tablas.rendimientos[[i]][5,2]
  hitratio.randomForest[i]<- tablas.rendimientos[[i]][5,3]
  hitratio.EW16<- tablas.rendimientos[[i]][5,4]
  hitratio.IPSA<- tablas.rendimientos[[i]][5,5]
}


hitratio.dataset<- as.data.frame(cbind(hitratio.htrees, hitratio.baggedTrees, hitratio.randomForest, hitratio.EW16, hitratio.IPSA))
colnames(hitratio.dataset)<- c("Dec.TreeHyper", "BaggedTrees", "RandomForest", "EW16", "IPSA")
head(hitratio.dataset)
mostrando.hitratio<- matrix(c(100*sapply(hitratio.dataset, mean, na.rm=TRUE), 100*sapply(hitratio.dataset, var, na.rm=TRUE), sapply(hitratio.dataset, skewness, na.rm=TRUE), sapply(hitratio.dataset, kurtosis, na.rm=TRUE)), nrow=4, ncol=5, byrow=TRUE)
colnames(mostrando.hitratio)<- c("Dec.TreeHyper", "BaggedTrees", "RandomForest", "EW16", "IPSA")
rownames(mostrando.hitratio)<- c("Media en %", "Varianza en %", "Asimetr?a", "Curtosis")
mostrando.hitratio


#OCIO
colMeans(tabla.ocio)
hist(tabla.ocio[,1])
hist(tabla.ocio[,2])
hist(tabla.ocio[,3])



#Metricas valiosas
plot(SP_IPSA_SN$Precio)
hist(retornos.htrees)
hist(retornos.baggedTrees)
hist(retornos.randomForest)
hist(retornos.EW16)
hist(retornos.IPSA)
boxplot(dataset.resultados, las=2, col= NULL,cex.axis=0.7)

mostrando.rendimientos
mostrando.hitratio
mostrando.turnovers
sharpe.tanteo
tracking.error
information.ratio

shapiroTest(retornos.htrees)
shapiroTest(retornos.baggedTrees)
shapiroTest(retornos.randomForest)
shapiroTest(retornos.EW16)
shapiroTest(retornos.IPSA)


100*CI(retornos.htrees, ci=0.95)
100*CI(retornos.baggedTrees, ci=0.95)
100*CI(retornos.randomForest, ci=0.95)
100*CI(retornos.EW16, ci=0.95)
100*CI(retornos.IPSA, ci=0.95)

