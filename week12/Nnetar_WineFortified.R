library(readr)
library(forecast)
library(caret)
library(ggplot2)
AustralianWines <- read_csv("~/Documents/\U0001f4d4BAFT_2018/week5/HW_AustralianWines/AustralianWines.csv", 
                            locale = locale(encoding = "BIG5"))
# Define object (fortified wine)
nvalid=12
ntrain=length(AustralianWines$Month)-nvalid
fortified.ts<-ts(AustralianWines$Fortified,start = c(1980,1),frequency = 12)
train.ts <- ts(AustralianWines$Fortified[1:ntrain],start = c(1980,1),frequency = 12)
valid.ts <- ts(AustralianWines$Fortified[(ntrain+1):(ntrain+nvalid)],start = c(1994,1),frequency = 12)
## Neural net: nnetar
nn1 <- nnetar(train.ts,p=11)
trainNNF<-nn1$fitted
trainNNE<-nn1$residuals
nn1.pred <- predict(nn1, newdata = valid.ts)
validNNF<-ts(nn1.pred$mean[1:12],start=c(1994,1),frequency = 12)
validNNE<-valid.ts-validNNF
## Performance Charts | NN
# Accuracy
accuracy(train.ts,trainNNF)
accuracy(valid.ts,validNNF)

# Actual & forecasted series
autoplot(train.ts,series = 'Data')+
  forecast::autolayer(trainNNF, series = 'NN', linetype = 'dashed')+
  xlab("Time") + ylab("Wine Sales") +
  ggtitle('Actual & forecasted series |NN')+
  guides(colour=guide_legend(title="Series")) +
  scale_color_manual(values=c(Data="darkblue", NN="red"))
# Forecast errors series (training and validation)
autoplot(trainNNE, series='NN')+
  ggtitle("Forecast Errors") +
  xlab("Time") + ylab("Wine Saless") +
  ggtitle('Forecasted Error |NN')+
  guides(colour=guide_legend(title="Series")) +
  scale_color_manual(values=c(NN="red"))

## Exponential smoothing mode
etsfit<-ets(train.ts)
ets.pred<-forecast(etsfit,h=nvalid)
trainetsF<-ets.pred$fitted
trainetsE<-train.ts-trainetsF
validetsF<-ets.pred$mean
validetsE<-valid.ts-ets.pred$mean
## Performance Charts |NN ETS
# Accuracy
accuracy(train.ts,trainetsF)
accuracy(valid.ts,validetsF)

# Actual & forecasted series
autoplot(fortified.ts,series = 'Data',alpha=0.2)+
  forecast::autolayer(trainNNF, series = 'NN',alpha=0.5)+
  forecast::autolayer(trainetsF, series = 'ETS',alpha=0.5)+
  forecast::autolayer(validNNF, series = 'NN', linetype = 'dashed',alpha=0.5)+
  forecast::autolayer(validetsF, series = 'ETS', linetype = 'dashed',alpha=0.5)+
  xlab("Time") + ylab("Wine Sales") +
  ggtitle('Actual & forecasted series|NN ETS')+
  guides(colour=guide_legend(title="Series")) +
  scale_color_manual(values=c(Data="black", NN="red",ETS='blue'))
# Forecast errors series (training and validation)
autoplot(trainNNE, series='NN')+
  forecast::autolayer(trainetsE, series = 'ETS')+
  forecast::autolayer(validNNE, series = 'NN', linetype = 'dashed')+
  forecast::autolayer(validetsE, series = 'ETS', linetype = 'dashed')+
  ggtitle("Forecast Errors") +
  xlab("Time") + ylab("Wine Saless") +
  guides(colour=guide_legend(title="Series")) +
  scale_color_manual(values=c(NN="red",ETS='blue'))
