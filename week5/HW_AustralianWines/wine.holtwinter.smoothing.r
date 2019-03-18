###short-term forecast for 6 series,and repeat every month
#library
library(readr)
library(forecast)
library(ggplot2)
#read data
Wine<-read_csv("AustralianWines.csv", 
               locale = locale(encoding = "BIG5"))
mean(Wine$Fortified)
length(Wine$Month)
#defind object fortified 
fortified.ts<-ts(Wine$Fortified,c(1980,1),frequency=12)
nvalid<-12
ntrain<-length(fortified.ts)-nvalid
train.ts<-window(fortified.ts,start=c(1980,1),end=c(1980,ntrain))
valid.ts<-window(fortified.ts,start=c(1980,ntrain+1),end=c(1980,ntrain+nvalid))

#Holt-Winter’s exponential smoothing 
#(with multiplicative seasonality)
plot(train.ts)
mean(fortified.ts)
hwin<-ets(train.ts,model='ZZM')
hwin.pred<-forecast(hwin,h=nvalid,level = 95)
autoplot(hwin)
validF<-hwin.pred$mean
validE<-valid.ts-hwin.pred$mean
trainF<-hwin.pred$fitted
trainE<-hwin.pred$residuals

autoplot(fortified.ts, series='Data') +
  autolayer(trainF, series='hwin') +
  autolayer(validF, series='hwin', linetype = 'dashed') +
  ggtitle("Actuals & Forecasts") +
  xlab("Time") + ylab("Wine Sales") +
  guides(colour=guide_legend(title="Series")) +
  scale_color_manual(values=c(Data="black", Naïve="blue", SeasonalNaïve="red"))
#performance check
accuracy(trainF,train.ts)
accuracy(validF,valid.ts)
#(train+valid) for 
hwinwine<-ets(fortified.ts,model = 'ZZM')
hwinwine.pred<-forecast(hwinwine,h=2,level = 95)
hwinwine.pred
#visualization residuals
autoplot(hwinwine.pred$residuals)+
  ylab('residuals')+
  ggtitle("Fortified Forecasts") 
accuracy(fortified.ts,hwinwine.pred$fitted)
perf <-  cbind(fitted = c(hwinwine.pred$fitted),
               errors = c(hwinwine.pred$residuals))
perform <- cbind(Wine,perf)     
write.csv(perform, file="wineperf.csv", na = "")
