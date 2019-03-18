#library
library(readr)
library(forecast)
library(ggplot2)
#load data
Wines <- read_csv("AustralianWines.csv")

#define object
Wine.ts<-ts(Wines$Fortified,start = c(2001,1),frequency=12)
nvalid=12
ntrain=length(Wine.ts)-nvalid
train.ts=window(Wine.ts,start=c(2001,1),end=c(2001,ntrain))
valid.ts=window(Wine.ts,start=c(2001,ntrain+1),end=c(2001,ntrain+nvalid))
autoplot(Wine.ts)

# training forecast:expo trend and monthly predictors
train.lm.expo.season<-tslm(train.ts~trend+season,lambda = 0)
train.lm.expo.season
summary(train.lm.expo.season)

train.lm.log.season.pred=forecast(train.lm.log.season,h=12)

validF <- train.lm.log.season.pred$mean
validE <- log(valid.ts) - train.lm.log.season.pred$mean
trainF <- train.lm.log.season.pred$fitted
trainE <- train.lm.log.season.pred$residuals


## Performance 
accuracy(valid.ts,exp(validF))
#perf chart Forecast 
plot(Wine.ts)
lines(exp(trainF),col='red')
lines(exp(validF),col='red',lty=3)
#perf chart Error
plot(valid.ts-exp(validF),col='red')


# train+valid forecast:log(y) linear trend == y expo trend
lm.expo.season<-tslm(Wine.ts~trend+season,lambda = 0)
lm.expo.season.pred=forecast(lm.expo.season,h=2)
lm.expo.season.pred
