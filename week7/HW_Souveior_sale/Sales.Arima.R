#library
library(readr)
library(forecast)
library(ggplot2)
# Read data
SouvenirSales <- read_csv("SouvenirSales.csv")
#define object
Sales.ts<-ts(SouvenirSales$Sales,start = c(1995,1),frequency=12)
nvalid=12
ntrain=length(Sales.ts)-nvalid
train.ts=window(Sales.ts,start=c(1995,1),end=c(1995,ntrain))
valid.ts=window(Sales.ts,start=c(1995,ntrain+1),end=c(1995,ntrain+nvalid))

## model:forecast:linear trend and monthly predictors
train.lm <- tslm(train.ts ~ trend + season,lambda = 0)
# train forecast
train.lm.pred<-forecast(train.lm,h=12)
forecast(train.lm,h=1)

# sale forecast
forecast.lm<-tslm(Sales.ts ~ trend + season,lambda = 0)
forecast.lm.pred<-forecast(forecast.lm,h=12)
forecast(forecast.lm,h=2)

trainF<-train.lm.pred$fitted
trainE<-train.ts-trainF
validF<-train.lm.pred$mean
validE<-valid.ts-train.lm.pred$mean
train.ts
(1-1/exp(train.lm.pred$residuals))*train.ts
trainE

ForecastF<-forecast.lm.pred$mean
ForecastE<-Sales.ts-forecast.lm.pred$fitted
##ARIMA for residuals
# ACF for lag 15
Acf(trainE,lag.max = 15, main='')

# ARIMA for error lag-2 train.ts
train.lm.res.arima=Arima(trainE,order = c(2,0,0))

summary(train.lm.res.arima)
Acf(train.lm.res.arima$residuals,lag.max = 15, main='')

# ARIMA for error lag-2 forecast.ts
forecast.lm.res.arima=Arima(ForecastE,order = c(2,0,0))
# forecast
train.lm.res.arima.pred<-forecast(train.lm.res.arima,h=12)
forecast.lm.res.arima.pred<-forecast(forecast.lm.res.arima,h=12)

## model + Arima
trainF_A<-trainF+train.lm.res.arima.pred$fitted
ValidF_A<-validF+train.lm.res.arima.pred$mean
ForecastF_A<-ForecastF+forecast.lm.res.arima.pred$mean
ValidF_A
ForecastF_A
## Performance 
accuracy(valid.ts,validF)
accuracy(valid.ts,ValidF_A)
#perf chart Forecast 
plot(Sales.ts)
lines(trainF,col='blue')
lines(validF,col='blue',lty=3)
lines(trainF_A,col='red')
lines(ValidF_A,col='red',lty=3)

