library(forecast)
library(readr)
# reading data
SIG_data <- read_csv("SIG data.csv")

# making data as time series and plotting them
SIG.158.ts = ts(SIG_data[,159],start = c(2014,1), end = c(2017, 12), freq = 12)
length(SIG.158.ts)
plot.ts(SIG.158.ts, ylab = 'SIG.158')
acf(SIG.158.ts, main = " SIG.158")


nValid <- 12
nTrain <- length(SIG.158.ts) - nValid
train.ts <- window(SIG.158.ts, start = c(2014, 1), end = c(2014, nTrain))
valid.ts <- window(SIG.158.ts, start = c(2014, nTrain + 1), end = c(2014, nTrain + nValid))
## Linear model and ACF plot for residuals
# fitting linear model
train.lm.trend.season<-tslm(train.ts~trend+I(trend^2)+season)
summary(train.lm.trend.season)
# forecasting using linear model
trac.pred <- forecast(train.lm.trend.season,h = length(valid.ts)) 
# plotting the forecasting results
plot(trac.pred, ylab = "Ridership", xlab = "Year", 
     bty = "l", xaxt = "n", main ="", flty = 2) 
axis(1, at = seq(1991,2004, 1), labels = format(seq(1991, 2004, 1))) 
lines(trac.pred$fitted, lwd = 2, col = "blue") 
lines(valid.ts)
accuracy(trac.pred,valid.ts)
# plotting linear model residuals
plot(trac.pred$residuals,lwd=2,col="blue",ylab="Residuals")
# ACF for model residuals
acf(ts(trac.pred$residuals),main='ACF Ridership', lag.max =12)

# fitting linear model
train.lm.trend.season<-tslm(train.ts~trend+I(trend^2)+season)
# fitting AR(1) to residuals
train.res.arima <- Arima(train.lm.trend.season$residuals, order = c(1,0,0))
# forecasting residuals
train.res.arima.pred <- forecast(train.res.arima, h =length(valid.ts))
# plotting residuals and their forecast series
plot(train.lm.trend.season$residuals, ylab = "Residuals",
     xlab = "Year", bty = "l", xaxt = "n", xlim = c(1991, 2004.25), main = "")
axis(1, at = seq(1991, 2004, 1), labels = format(seq(1991, 2004, 1)))
lines(train.res.arima.pred$fitted, lwd = 2, col = "blue")
summary(train.res.arima)
# ACF plot for the residuals
acf(ts(train.res.arima.pred$residuals),main='ACF residuals-of-residuals')
### Autoregressive Integrated moving average models (ARIMA)
# fitting ARIMA model
train.arima = auto.arima(train.ts)
summary(train.arima)
# plotting results
arima.fore = forecast(train.arima, h= length(valid.ts))
plot(error)
# plotting the forecasting results
plot(arima.fore, ylab = "Ridership", xlab = "Year", 
     bty = "l", xaxt = "n", main ="", flty = 2) 
axis(1, at = seq(1991,2004, 1), labels = format(seq(1991, 2004, 1))) 
lines(arima.fore$fitted, lwd = 2, col = "blue") 
lines(valid.ts)
# ACF for ARIMA residuals
acf(ts(train.arima$residuals),main='ACF ARIMA Residual')
