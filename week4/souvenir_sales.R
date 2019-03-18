## Load libraries
library(forecast)
library(ggplot2)
library(lubridate)

## Read data from CSV and create a dataframe

# SouvenirSales.csv has data on sale every month for 6 years
souenvir_df<-read.csv("SouvenirSales.csv",header = TRUE)

# check to see it loaded properly:
head(souenvir_df)
summary(souenvir_df)

## Manipulate the dataframe (useful for forecasting tasks) 
# e.g. create timestamp column, split series into multiple sub-series, change aggregation level

# Change the DATE format
souenvir_df$Date = ymd(paste(souenvir_df$Date))

## Define ts object
souenvir.ts <- ts(souenvir_df$Sales, start = c(1,1), freq = 12)  
# this example uses 1 year as cycle. Every year has 12 periods;  start is on period 1 of month 1. 


autoplot(souenvir.ts, xlab = "Time", ylab = "Sales")
# useful for a quick view of the time series. You can replace autoplot() with plot()
# to create a time plot with meaningful DATE-TIME on x-axis (type=͟l means  line chart):
plot(x=souenvir_df$Date,y=souenvir.ts, type="l")

plot(x=souenvir_df$Date,y=valid.ts, type="l")

## Partition the time series into training and validation 
valid.ts <- subset(souenvir.ts, start = length(souenvir.ts) - 12 +1) #extracts last week of data
train.ts <- subset(souenvir.ts, end = length(souenvir.ts) - length(valid.ts))



#naive
nValid <- length(valid.ts)
fit <- naive(train.ts, h=nValid)
validNaiveF <- fit$mean
validNaiveE <- valid.ts - fit$mean
trainNaiveF <- fit$fitted
trainNaiveE <- fit$residuals
#snaive
nValid <- length(valid.ts)
fit <- snaive(train.ts, h=nValid) #h= number of time want to forecast
validSnaiveF <- fit$mean
validSnaiveE <- valid.ts - fit$mean
trainSnaiveF <- fit$fitted
trainSnaiveE <- fit$residuals

## Performance Measures

#Compute performance measures (RMSE, MAPE, etc.) for the training and validation periods 
accuracy(fit,valid.ts)
accuracy(fit,valid.ts)
## Performance Charts
#hist of validNaiveE
hist(validNaiveE,breaks = 12)
qplot(validNaiveE, geom="histogram",main='validNaiveE',col=I('white') )
qplot(validSnaiveE, geom="histogram",main='validSnaiveE',col=I('white') )

# Actual & forecasted series
autoplot(souenvir.ts, series='Data') +
  autolayer(trainSnaiveF, series='Snaïve') +
  autolayer(validSnaiveF, series='Snaïve', linetype = 'dashed') +
  ggtitle("Actuals & Forecasts") +
  xlab("Time") + ylab("Souenvir Sales") +
  guides(colour=guide_legend(title="Series")) +
  scale_color_manual(values=c(Data="black", Snaïve="blue"))


# Forecast errors series (training and validation)
autoplot(trainNaiveE, series='Naïve')+
  autolayer(validNaiveE, series='Naïve', linetype = 'dashed') +
  autolayer(validSnaiveE, series='SeasonalNaïve', linetype = 'dashed') +
  autolayer(trainSnaiveE, series='SeasonalNaïve') +
  ggtitle("Forecast Errors") +
  xlab("Time") + ylab("Soivenir Sales") +
  guides(colour=guide_legend(title="Series")) +
  scale_color_manual(values=c(Naïve="blue", SeasonalNaïve="red"))


# Combine all series and export to CSV (e.g., for plotting in Tableau)

perf <-  cbind(naive.fitted = c(trainNaiveF, validNaiveF),
               naive.errors = c(trainNaiveE, validNaiveE),
               snaive.fitted = c(trainSnaiveF, validSnaiveF),
               snaive.errors = c(trainSnaiveE, validSnaiveE))
perform <- cbind2(souenvir_df, perf)     
write.csv(perform, file="souenvir.csv", na = "")





