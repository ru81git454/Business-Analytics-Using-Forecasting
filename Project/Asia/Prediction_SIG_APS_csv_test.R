sig_pivot <- read.csv("APS_clean_version.csv",
               header = TRUE,
               row.names = 1)
#########data processing########
library(forecast)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(zoo)

bestF_sheet<<-matrix(c('customer','model name','RMSE',as.character(1:12)))
bestE_sheet<<-matrix(c('customer','model name','RMSE',as.character(1:12)))

#######model########
#seasonnaive model 1
snaive_model<-function(t,v){
  Snaive.fore <- snaive(t, h=12)
  validSnaiveF <<- Snaive.fore$mean
  validSnaiveE <<- v - Snaive.fore$mean
  trainSnaiveF <<- Snaive.fore$fitted
  trainSnaiveE <<- Snaive.fore$residuals
}
#ets model 2
ets_model<-function(t,v){
  hwin <- ets(t)
  hwin.fore  <-forecast(hwin,h=12)
  validhwinF <<-hwin.fore$mean
  validhwinE <<-v-hwin.fore$mean
  trainhwinF <<- hwin.fore$fitted
  trainhwinE <<- hwin.fore$residuals
}
#arima model 3 
arima_model<-function(t,v){
  arimaM<-auto.arima(t)
  arima.fore<-forecast(arimaM,h=12)
  validarimaF<<-arima.fore$mean
  validarimaE<<-v-validarimaF
  trainarimaF<<- arima.fore$fitted
  trainarimaE<<- arima.fore$residuals
}
## ma model 5
ma_model<-function(p,v){
  package.ma=rollmean(p,k=12,align='right')
  packageMAF<<-package.ma
  packageMAE<<-p-packageMAF
  validMAF<<-tail(package.ma,12)
  validMAF<<-ts(validMAF,start = c(2018,1),frequency = 12)
  validMAE<<-v-validMAF
}
## tslm model 5
lm_model<-function(t,v){
  lmM<-tslm(t~trend+season)
  lm.fore<-forecast(lmM,h=12)
  validlmF<<-lm.fore$mean
  validlmE<<-v-validlmF
  trainlmF <<- lm.fore$fitted
  trainlmE <<- lm.fore$residuals
}

#######model_compare########
sig_pred_actual_matrix<-as.matrix(sig_pred_actual)
for (i in 1:length(sig_pred_actual)){
  nam <- paste(nams[i,])
  #define object
  package.ts <-ts(SIG_data[,i], start = c(2009,1), freq =12)
  nvalid=12
  ntrain=length(package.ts)-nvalid
  valid.ts <- subset(package.ts, start = length(package.ts) - 12+1)
  train.ts <- subset(package.ts, end = length(package.ts) - length(valid.ts))
  #seasonnaive model 1
  snaive_model(train.ts,valid.ts)
  #ets model 2
  ets_model(train.ts,valid.ts)
  #arima model 3 
  arima_model(train.ts,valid.ts)
  ##ma model 
  ma_model(train.ts,valid.ts)
  ## tslm model 5
  lm_model(train.ts,valid.ts)
  
  ##model select_accuracy
  modelsF<-list(validSnaiveF,validhwinF,validarimaF,validMAF,validlmF)
  modelsE<-list(validSnaiveE,validhwinE,validarimaE,validMAE,validlmE)
  modelsNames<-c('Snaive','hwin','arima','MA','lm')
  accuracylists<-list(accuracy(valid.ts,validSnaiveF)[2],
                      accuracy(valid.ts,validhwinF)[2],
                      accuracy(valid.ts,validarimaF)[2],
                      accuracy(valid.ts,validMAF)[2],
                      accuracy(valid.ts,validlmF)[2] )
  modelname<-modelsNames[which.min(accuracylists)]
  bestmodelF<-unlist(modelsF[which.min(accuracylists)])
  bestmodelE<-unlist(modelsF[which.min(accuracylists)])
  bestAccuracy<-unlist(accuracylists[which.min(accuracylists)])
  bestF<-c(nam,modelname,bestAccuracy,bestmodelF)
  bestE<-c(nam,modelname,bestAccuracy,bestmodelE)
  bestF_sheet=cbind(bestF_sheet,as.data.frame(bestF))
  bestE_sheet=cbind(bestE_sheet,as.data.frame(bestE))
}


################output##################
best_model_datasets<-list('data'=data_w_nams,'bestF_sheet'=bestF_sheet,'bestE_sheet'=bestE_sheet,'bestF_sheet_2017'=bestF_sheet_2017,'bestE_sheet_2017'=bestE_sheet_2017)
write.xlsx(best_model_datasets, 'best_model_datasets.xlsx')

i=1
##############Performance chart#############
####model performance ###
plotcompare<-function(i){
  nam <- paste(nams[i,])
  #define object
  package.ts <-ts((SIG_data[,i]), start = c(2009,1), freq =12)
  nvalid=12
  ntrain=length(package.ts)-nvalid
  train.ts <- window(package.ts, start=c(2009,1),end=c(2009,ntrain))
  valid.ts <- window(package.ts, start=c(2009,ntrain+1),end = c(2009,ntrain+nvalid))
  
  #seasonnaive model 1
  snaive_model(train.ts,valid.ts)
  #ets model 2
  ets_model(train.ts,valid.ts)
  #arima model 3 
  arima_model(train.ts,valid.ts)
  #
  ma_model(package.ts,valid.ts)
  #tslm
  lm_model(train.ts,valid.ts)
  
  autoplot(package.ts, series='Data',alpha=0.2) +
    forecast::autolayer(validSnaiveF, series='Snaive', linetype = 'dashed',alpha=0.5) +
    forecast::autolayer(validhwinF, series='hwin', linetype = 'dashed',alpha=0.5) +
    forecast::autolayer(validarimaF, series='arima', linetype = 'dashed',alpha=0.5) +
    forecast::autolayer(validMAF, series='MA',alpha=0.5) +
    forecast::autolayer(validlmF, series='lm',alpha=0.5) +
    ggtitle(paste("three model Forecast",nam)) +
    xlab("Time") + ylab("Sales") +
    guides(colour=guide_legend(title="series")) +
    scale_color_manual(values=c(Data="black", Snaive="orange",hwin='red',arima='blue',MA='yellow',lm='brown'))
}
for (i in 1:86){
  plotcompare(i)
}
plotcompare(38)
i=5
nam <- nams[i,]
match(nam ,nams$ID)
plotcompare<-function(i){
  nam <- paste(nams[i,])
  #define object
  package.ts <-ts((SIG_data[,i]), start = c(2009,1), freq =12)
  nvalid=12
  ntrain=length(package.ts)-nvalid
  train.ts <- window(package.ts, start=c(2009,1),end=c(2009,ntrain))
  valid.ts <- window(package.ts, start=c(2009,ntrain+1),end = c(2009,ntrain+nvalid))
  
  #seasonnaive model 1
  snaive_model(train.ts,valid.ts)
  #ets model 2
  ets_model(train.ts,valid.ts)
  #arima model 3 
  arima_model(train.ts,valid.ts)
  #
  ma_model(package.ts,valid.ts)
  #tslm
  lm_model(train.ts,valid.ts)
  
  autoplot(package.ts, series='Data',alpha=0.2) +
    forecast::autolayer(validSnaiveF, series='Snaive', linetype = 'dashed',alpha=0.5) +
    forecast::autolayer(validhwinF, series='hwin', linetype = 'dashed',alpha=0.5) +
    forecast::autolayer(validarimaF, series='arima', linetype = 'dashed',alpha=0.5) +
    forecast::autolayer(validMAF, series='MA',alpha=0.5) +
    forecast::autolayer(validlmF, series='lm',alpha=0.5) +
    ggtitle(paste("three model Forecast",nam)) +
    xlab("Time") + ylab("Sales") +
    guides(colour=guide_legend(title="series")) +
    scale_color_manual(values=c(Data="black", Snaive="orange",hwin='red',arima='blue',MA='yellow',lm='brown'))
}
