#library
library(readxl)
library(dplyr)
library(ggplot2)
library(forecast)
# read data
Sept11<- read_excel("Sept11Travel.xls")

##time series regression
#define ts object
Air.ts<-ts(Sept11$`Air RPM (000s)`,c(1990,1),c(2004,4),frequency = 12)
autoplot(Air.ts)
nvalid=16
ntrain=length(Air.ts)-nvalid
train.ts=window(Air.ts,start=c(1990,1),end=c(1990,ntrain))
valid.ts=window(Air.ts,start=c(1990,ntrain+1),end=c(1990,ntrain+nvalid))
#create external predictor 1=afterattack
Sept11$Attack<-if_else(Sept11$Month > '2001-09-11',1,0)

##A:Air reg model w/o predictor
#A.forecast
Air.tslm<-tslm(train.ts~trend+season)
Air.tslm.fore<-forecast(Air.tslm,h=nvalid)
A.trainF<-Air.tslm.fore$fitted
A.validF<-Air.tslm.fore$mean
A.trainE<-Air.tslm.fore$residuals
A.validE<-valid.ts-Air.tslm.fore$mean
autoplot(Air.tslm.fore)
##B:Air reg model w/ predictor
#Remove Seasonality(for check)
Air.stl<-stl(train.ts,s.window = 'periodic')
autoplot(Air.stl,main='decomponent of Airline Data')
#dummy-afterattack
xtrain=Sept11$Attack[1:ntrain]
xvalid=Sept11$Attack[(ntrain+1):(ntrain+nvalid)]
#B.forecast
Air.stlm<-stlm(train.ts,s.window = 'periodic',xreg = xtrain,method = 'arima')
Air.stlm.fore<-forecast(Air.stlm,h=nvalid,xreg=xvalid)
B.trainF<-Air.stlm.fore$fitted
B.validF<-Air.stlm.fore$mean
B.trainE<-Air.stlm.fore$residuals
B.validE<-valid.ts-Air.stlm.fore$mean
autoplot(Air.stlm.fore)

###performance
accuracy(A.validF,valid.ts)
accuracy(B.validF,valid.ts)

#ts-plot
autoplot(Air.ts, series='Data',alpha=0.2) +
  autolayer(A.trainF, series='w/o',alpha=0.5) +
  autolayer(A.validF, series='w/o', linetype = 'dashed',alpha=0.5) +
  autolayer(B.trainF, series='w/',alpha=0.5) +
  autolayer(B.validF, series='w/', linetype = 'dashed',alpha=0.5) +
  ggtitle("Airline Data Actuals & Forecasts") +
  xlab("Time") + ylab("Air passenger miles") +
  guides(colour=guide_legend(title="Series")) +
  scale_color_manual(values=c(Data="black", 'w/o'="dark green",'w/'='red'))
#error-plot
autoplot(A.trainE, series='w/o')+
  autolayer(A.validE, series='w/o', linetype = 'dashed',alpha=0.5) +
  autolayer(B.validE, series='w/', linetype = 'dashed',alpha=0.5) +
  autolayer(B.trainE, series='w/') +
  ggtitle("Airline Data Errors") +
  xlab("Time") + ylab("Air passenger miles") +
  guides(colour=guide_legend(title="Series")) +
  scale_color_manual(values=c('w/'="red", 'w/o'="dark green"))




