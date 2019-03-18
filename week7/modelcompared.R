library(forecast)
# model comparing
for (i in length(data)-1){
  nam <- paste(colnames(data[i+1]))
  #define object
  package.ts <-ts(data[i+1], start = c(2014,1), freq =12)
  valid.ts <- subset(package.ts, start = 37)
  train.ts <- subset(package.ts, end = 36)
  #seasonnaive model
  Snaive.fore <- snaive(train.ts, h=12)
  validSnaiveF <- Snaive.fore$mean
  validSnaiveE <- valid.ts - Snaive.fore$mean
  trainSnaiveF <- Snaive.fore$fitted
  trainSnaiveE <- Snaive.fore$residuals
  Snaive.accuracy<-accuracy(Snaive.fore,valid.ts)
  
  #ets model
  hwin <- ets(train.ts)
  hwin.fore<-forecast(hwin,h=12)
  validhwinF<-hwin.fore$mean
  validhwinE<-valid.ts-hwin.fore$mean
  trainhwinF <- hwin.fore$fitted
  trainhwinE <- hwin.fore$residuals
  hwin.accuracy<-accuracy(hwin.fore,valid.ts)
  
  #arima
  arima<-auto.arima(train.ts)
  arima.fore<-forecast(arima,h=12)
  validarimaF<-arima.fore$mean
  validarimaE<-valid.ts-arima.fore$mean
  trainarimaF <- arima.fore$fitted
  trainarimaE <- arima.fore$residuals
  arima.accuracy<-accuracy(arima.fore,valid.ts)
  
  #model performance 
  autoplot(package.ts, series='Data',alpha=0.2) +
    forecast::autolayer(trainSnaiveF, series='Snaive',alpha=0.5) +
    forecast::autolayer(validSnaiveF, series='Snaive', linetype = 'dashed',alpha=0.5) +
    forecast::autolayer(trainhwinF, series='hwin',alpha=0.5) +
    forecast::autolayer(validhwinF, series='hwin', linetype = 'dashed',alpha=0.5) +
    forecast::autolayer(trainarimaF, series='arima',alpha=0.5) +
    forecast::autolayer(validarimaF, series='arima', linetype = 'dashed',alpha=0.5) +
    ggtitle(paste("three model Errors",nam)) +
    xlab("Time") + ylab("Sales") +
    guides(colour=guide_legend(title="series")) +
    scale_color_manual(values=c(Data="black", Snaive="dark green",hwin='red',arima='blue'))
  #error-plot
  autoplot(trainSnaiveE, series='Snaive')+
    forecast::autolayer(validSnaiveE, series='Snaive', linetype = 'dashed',alpha=0.5) +
    forecast::autolayer(trainhwinE, series='hwin', linetype = 'dashed',alpha=0.5) +
    forecast::autolayer(validhwinE, series='hwin') +
    forecast::autolayer(trainarimaE, series='arima', linetype = 'dashed',alpha=0.5) +
    forecast::autolayer(validarimaE, series='arima') +
    ggtitle(paste("three model Errors =",nam)) +
    xlab("Time") + ylab("Air passenger miles") +
    guides(colour=guide_legend(title="Series")) +
    scale_color_manual(values=c(Data="black", Snaive="dark green",hwin='red',arima='blue'))
  }

