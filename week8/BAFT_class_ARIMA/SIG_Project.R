library(forecast)
## reading data
## First change format cells for series into number with two decimals 
SIG.information<-read.csv("SIG Europe data.csv",header = TRUE)[,(1:3)]
SIG.data<-t(read.csv("SIG Europe data.csv",header = FALSE)[,-(1:3)])
## take the two first character of the country name
SIG.information$Country_two<-substr(SIG.information$Country, start = 1, stop = 2)
## Combining customer codes and Country names
SIG.information$Column.name<-paste(SIG.information$Link..Cust...PH., SIG.information$Country_two, sep="")
## use that created column as column name for SIG.data
colnames(SIG.data)<-c("date",SIG.information$Column.name)
View(SIG.data)

##### Series  
SIG.13.ts = ts(as.numeric(SIG.data[,111]),start = c(2014,1), end = c(2017, 12), freq = 12)
length(SIG.13.ts)
plot.ts(SIG.13.ts, ylab = '13')
acf(ts(SIG.13.ts),main='ACF 13') ## Cyclical pattern 
                                ## Positive lag-1 autocorrelation 
## Training and Validation 
# nValid <- 12
# nTrain <- length(SIG.1.ts) - nValid
train.ts <- window(SIG.13.ts, start = c(2014, 1), end = c(2016, 12))
valid.ts <- window(SIG.13.ts, start = c(2017, 1), end = c(2017, 12))

#####################
# fitting linear model
####################
train.lm.trend.season<-tslm(train.ts~trend+season)
summary(train.lm.trend.season)

# forecasting using linear model
trac.pred <- forecast(train.lm.trend.season,h = length(valid.ts)) 
# plotting the forecasting results
plot(trac.pred, ylab = "13", xlab = "Year", 
     bty = "l", xaxt = "n", main ="", flty = 2) 
axis(1, at = seq(2014,2018, 1), labels = format(seq(2014, 2018, 1))) 
lines(trac.pred$fitted, lwd = 2, col = "blue") 
lines(valid.ts)

accuracy(trac.pred,valid.ts)

# plotting linear model residuals
plot(trac.pred$residuals,lwd=2,col="blue",ylab="Residuals")

# ACF for model residuals
acf(ts(trac.pred$residuals),main='ACF 251')

############################## 
#ARIMA for multiple time series
##############################
# 13 (2,1,0)
# 14 (0,0,1) # Group 6 
# 15 (0,1,0)
# 61 (1,0,0) # Group 1
# 76 (1,0,0) # Group 2
# 81 (3,1,1)
# 110 (1,0,0) # Group 3
# 121 (1,0,0) # Group 4
# 128 (0,0,1) # Group 7
# 140 (1,0,0)
# 152 (2,1,1)
# 154 (1,0,0)
# 158 (1,0,0) # Group 5
# 170 (2,1,1)
# 199 (0,1,1)
# 200 (0,1,1)
# 204 (0,0,1) # Group 8
#### 13
##### Series  
SIG.13.ts = ts(as.numeric(SIG.data[,14]),start = c(2014,1), end = c(2017, 12), freq = 12)
length(SIG.13.ts)
plot.ts(SIG.13.ts, ylab = '13')
acf(ts(SIG.13.ts),main='ACF 13') ## Cyclical pattern 
## Positive lag-1 autocorrelation 
## Training and Validation 
# nValid <- 12
# nTrain <- length(SIG.1.ts) - nValid
train.ts <- window(SIG.13.ts, start = c(2014, 1), end = c(2016, 12))
valid.ts <- window(SIG.13.ts, start = c(2017, 1), end = c(2017, 12))
# fitting ARIMA model
train.arima = auto.arima(train.ts)
summary(train.arima)
# plotting results
arima.fore = forecast(train.arima, h= length(valid.ts))
# plotting the forecasting results
plot(arima.fore, ylab = "13", xlab = "Year", 
     bty = "l", xaxt = "n", main ="", flty = 2) 
axis(1, at = seq(2014,2018, 1), labels = format(seq(2014, 2018, 1))) 
lines(arima.fore$fitted, lwd = 2, col = "blue") 
lines(valid.ts)
# ACF for ARIMA residuals
acf(ts(train.arima$residuals),main='ACF ARIMA Residual')


#### multiple ARIMA with chain
# All series are ARIMA (1,0,0)
series_no<-c(61,76,110,121,158)
m<-c()
for(i in series_no){
  SIG.ts<-ts(as.numeric(SIG.data[,i+1]),start = c(2014,1), end = c(2017, 12), freq = 12)
  train.ts <- window(SIG.ts, start = c(2014, 1), end = c(2016, 12))
  valid.ts <- window(SIG.ts, start = c(2017, 1), end = c(2017, 12))
  m<-c(train.ts,rep(NA,36),m)
  }
combined<-ts(m)
fit.combined <- auto.arima(combined)
summary(fit.combined)

for(i in series_no){
  SIG.ts<-ts(as.numeric(SIG.data[,i+1]),start = c(2014,1), end = c(2017, 12), freq = 12)
  train.ts <- window(SIG.ts, start = c(2014, 1), end = c(2016, 12))
  valid.ts <- window(SIG.ts, start = c(2017, 1), end = c(2017, 12))
  fit<-Arima(train.ts,model=fit.combined)
  fc <- forecast(fit, h =length(valid.ts))
  plot(fc, ylab = "", xlab = "Year", 
       bty = "l", xaxt = "n", main ="", flty = 2) 
  axis(1, at = seq(2014,2018, 1), labels = format(seq(2014, 2018, 1))) 
  lines(fc$fitted, lwd = 2, col = "blue") 
  lines(valid.ts)
}

#### multiple ARIMA with chain
# All series are ARIMA (0,0,1)
series_no<-c(14,128,204)
m<-c()
for(i in series_no){
  SIG.ts<-ts(as.numeric(SIG.data[,i+1]),start = c(2014,1), end = c(2017, 12), freq = 12)
  train.ts <- window(SIG.ts, start = c(2014, 1), end = c(2016, 12))
  valid.ts <- window(SIG.ts, start = c(2017, 1), end = c(2017, 12))
  m<-c(train.ts,rep(NA,36),m)
}
combined<-ts(m)
fit.combined <- auto.arima(combined)
summary(fit.combined)

for(i in series_no){
  SIG.ts<-ts(as.numeric(SIG.data[,i+1]),start = c(2014,1), end = c(2017, 12), freq = 12)
  train.ts <- window(SIG.ts, start = c(2014, 1), end = c(2016, 12))
  valid.ts <- window(SIG.ts, start = c(2017, 1), end = c(2017, 12))
  fit<-Arima(train.ts,model=fit.combined)
  fc <- forecast(fit, h =length(valid.ts))
  plot(fc, ylab = "", xlab = "Year", 
       bty = "l", xaxt = "n", main ="", flty = 2) 
  axis(1, at = seq(2014,2018, 1), labels = format(seq(2014, 2018, 1))) 
  lines(fc$fitted, lwd = 2, col = "blue") 
  lines(valid.ts)
}


