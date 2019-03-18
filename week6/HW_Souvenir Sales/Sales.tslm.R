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
# A:training forecast:linear trend and monthly predictors
train.lm.season<-tslm(train.ts~trend+season)
train.lm.season.pred<-forecast(train.lm.season,h=12)
summary(train.lm.season)
A.validF <- train.lm.season.pred$mean
A.validE <- valid.ts - train.lm.season.pred$mean
A.trainF <- train.lm.season.pred$fitted
A.trainE <- train.lm.season.pred$residuals
# B:training forecast:log(y) linear trend == y expo trend
train.lm.log.season<-tslm(log(train.ts)~trend+season)
train.lm.log.season
train.lm.expo.season<-tslm(train.ts~trend+season,lambda = 0)
train.lm.expo.season
summary(train.lm.expo.season)

train.lm.log.season.pred=forecast(train.lm.log.season,h=12)

B.validF <- train.lm.log.season.pred$mean
B.validE <- log(valid.ts) - train.lm.log.season.pred$mean
B.trainF <- train.lm.log.season.pred$fitted
B.trainE <- train.lm.log.season.pred$residuals

# train+valid forecast:log(y) linear trend == y expo trend
lm.expo.season<-tslm(Sales.ts~trend+season,lambda = 0)
lm.expo.season.pred=forecast(lm.expo.season,h=2)
lm.expo.season.pred
plot(exp(lm.expo.season))

## Performance 
accuracy(valid.ts,A.validF)
accuracy(valid.ts,exp(B.validF))
#perf chart Forecast 
plot(Sales.ts)
lines(A.trainF,col='blue')
lines(A.validF,col='blue',lty=3)
lines(exp(B.trainF),col='red')
lines(exp(B.validF),col='red',lty=3)


#perf chart Error
plot(A.validE,col='blue')
lines(valid.ts-exp(B.validF),col='red')

# Performance 
accuracy(train.ts,A.trainF)
accuracy(train.ts,exp(B.trainF))
#perf chart Error
plot(A.trainE,col='blue')
lines(train.ts-exp(B.trainF),col='red')
