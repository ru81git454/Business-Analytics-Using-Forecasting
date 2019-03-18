#library
library(readr)
library(zoo)
library(lubridate)
library(ggplot2)
#load data 
SIG.df <- read_csv("SIG data.csv")
head(SIG.df)
#change data format
#AustralianWines$Month=paste(ymd(AustralianWines$Month))
#define ts object
datalist.ts <- lapply(customers.df, function(t) ts(t, start=c(2014, 3), frequency=12))
class(datalist.ts[2])
#1. double-differencing 
fortified_diff<-diff(diff(Fortified.ts,lag =12),lag = 1)


#2.moving average(width=12) for double_diff forecasting.
ma.trailing_diff<-rollmean(fortified_diff,k=12,align = 'right')
last.ma.diff<-tail(ma.trailing_diff,1)
ma.trailing.diff.pred<-ts(rep(last.ma.diff,12),start=c(2016,1),end = c(2016,12),freq=12)
#3. Finally, adjust the forecast by “un-differencing” twice, in order to include back the trend and seasonality.
ma.trailing<-rollmean(Fortified.ts,k=12,align = 'right')
last.ma<-tail(ma.trailing,1)

ma.trailing.pred<-ts(rep(last.ma,12),start=c(2016,1),end = c(2016,12),freq=12)
#visual
Fortified_visual<-autoplot(Fortified.ts, series='Fortified') +
  autolayer(ma.trailing, series='Moving Average') +
  autolayer(ma.trailing.pred, series='Forecasting') +
  ggtitle("Wine Actuals & Forecasts") +
  xlab("Time") + ylab("Thousands of liters") +
  guides(colour=guide_legend(title="Series"))
Fortified_diff_visual<-autoplot(fortified_diff, series='Fortified_diff') +
  autolayer(ma.trailing_diff, series='Diff Moving Average') +
  autolayer(ma.trailing.diff.pred, series='Forecasting') +
  ggtitle("Wine Actuals & Forecasts(without Seasonality and Trend)") +
  xlab("Time") + ylab("Thousands of liters") +
  guides(colour=guide_legend(title="Series"))
Fortified_visual
Fortified_diff_visual
