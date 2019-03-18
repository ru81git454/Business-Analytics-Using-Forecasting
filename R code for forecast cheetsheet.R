## Load libraries

library(forecast)
library(ggplot2)
library(lubridate)

## Read data from CSV and create a dataframe

# bicup2006.csv has data on demand every 15-minutes for 3 weeks
souenvir_df<-read.csv("SouvenirSales.csv",header = TRUE)

bicup.df <- read.csv("bicup2006.csv")
# check to see it loaded properly:
head(bicup.df)


## Manipulate the dataframe (useful for forecasting tasks) 
# e.g. create timestamp column, split series into multiple sub-series, change aggregation level

# Combine the DATE and TIME columns into single DATETIME recognized by R (using lubridate package):
bicup.df$DATETIME = dmy_hm(paste(bicup.df$DATE, bicup.df$TIME))

# Extract only part of the series, say, weekdays:
bicup.weekday.df <- subset(bicup.df, weekdays(bicup.df$DATETIME)!="週六" & weekdays(bicup.df$DATETIME)!="週日")

# Aggregate the data into, say, daily demand:
bicup.daily.df <- aggregate(x=bicup.df$DEMAND, FUN=sum,  by= list(Date=day(bicup.df$DATETIME)))



## Define ts object

bicup.ts <- ts(bicup.df$DEMAND, start = c(1,1), freq = 63*7)  
# this example uses 1 week as cycle. Every day has 63 15-min periods;  start is on period 1 of week 1. 
# To change to 1-day cycle, change freq=63

autoplot(bicup.ts, xlab = "Time", ylab = "Demand")
# useful for a quick view of the time series. You can replace autoplot() with plot()

# to create a time plot with meaningful DATE-TIME on x-axis (type=͟l means  line chart):
plot(x=bicup.df$DATETIME, y=bicup.ts, type="l")

# Optional: create xts object for charts and data splitting (eg show DATETIME on chart x-axis, split by DOW)
# Note: xts doesn’t work with forecast package
library(xts)
bicup1 <- xts(bicup.df$DEMAND, order.by=bicup.df$DATETIME)
plot(bicup1)
weekdays <- bicup1[.indexwday(bicup1) %in% 1:5]   



## Partition the time series into training and validation 

# Option 1: using subset() to put the last 63*7 periods into validation; 
# To modify, change the number of periods 63*7
valid.ts <- subset(bicup.ts, start = length(bicup.ts) - 63*7 +1) #extracts last week of data
train.ts <- subset(bicup.ts, end = length(bicup.ts) - length(valid.ts))

# Option 2: using window() to put {day 15, period 1} to series end into validation 
# To modify this code, change c(15,1)
#valid.ts <- window(bicup.ts, start = c(15, 1), end = c(length(bicup.ts))
#train.ts <- subset(bicup.ts, end = length(bicup.ts) - length(valid.ts))
## Compute  forecasts and errors for the training and validation periods 
# (example uses naive, but you can modify to any other forecasting function)

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


## Performance Charts

# Actual & forecasted series
autoplot(bicup.ts, series='Data') +
  autolayer(trainNaiveF, series='Naïve') +
  autolayer(validNaiveF, series='Naïve', linetype = 'dashed') +
  autolayer(trainSnaiveF, series='SeasonalNaïve') +
  autolayer(validSnaiveF, series='SeasonalNaïve', linetype = 'dashed') +
  ggtitle("Actuals & Forecasts") +
  xlab("Time") + ylab("Num Passengers") +
  guides(colour=guide_legend(title="Series")) +
  scale_color_manual(values=c(Data="black", Naïve="blue", SeasonalNaïve="red"))


# Forecast errors series (training and validation)
autoplot(trainNaiveE, series='Naïve')+
  autolayer(validNaiveE, series='Naïve', linetype = 'dashed') +
  autolayer(validSnaiveE, series='SeasonalNaïve', linetype = 'dashed') +
  autolayer(trainSnaiveE, series='SeasonalNaïve') +
  ggtitle("Forecast Errors") +
  xlab("Time") + ylab("Num Passengers") +
  guides(colour=guide_legend(title="Series")) +
  scale_color_manual(values=c(Naïve="blue", SeasonalNaïve="red"))


# Combine all series and export to CSV (e.g., for plotting in Tableau)

perf <-  cbind(naive.fitted = c(trainNaiveF, validNaiveF),
               naive.errors = c(trainNaiveE, validNaiveE),
               snaive.fitted = c(trainSnaiveF, validSnaiveF),
               snaive.errors = c(trainSnaiveE, validSnaiveE))
perform <- cbind2(bicup.df, perf)     
write.csv(perform, file="bicupPerf.csv", na = "")





