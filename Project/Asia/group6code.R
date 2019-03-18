library(openxlsx)
library(forecast)
## read data
SIG.information<-read.xlsx(""SIG Europe data.xlsx"")[,(1:3)]
names(SIG.information)[1] <- ""Link.Cust_PH""
# time series start in column E
SIG.data<-t(read.xlsx(""SIG Europe data.xlsx"",colNames = FALSE)[,-(1:16)])  
## use new created column as column names for SIG.data
colnames(SIG.data)<-c(""date"",paste(SIG.information$Link.Cust_PH))


first_nonzero <- function(ts){
  for(i in 1:length(ts)){
    if(ts[i] > 0){
      break}
}
return(subset(ts, start = i))
}

zero_percentage <- function(ts){
  ts_first_nonzero <- first_nonzero(ts)
  z <- 0  
  for(i in 1:length(ts_first_nonzero)){
    if(ts_first_nonzero[i] == 0){ 
      z <-z +1}
    }
  return(z/length(ts_first_nonzero))
}

#return true -> forecase all zero
forecast_zero <- function(ts){
  last_6month_sale <-sum(subset(ts, start = length(ts) - 6+1))
  last_12month_sale <- sum(subset(ts, start = length(ts) - 12+1))
  if (last_6month_sale==0 |last_12month_sale <1000 |zero_percentage(ts)> 0.5){
    result <- TRUE
    } 
  else {
    result <- FALSE
    }
  return(result)
}

arima_forecast <- function (ts.in,n_valid=12,n_forecast=12){
  full.arima = auto.arima(ts.in)
  forecast_arima <- forecast(full.arima, h = n_forecast)
#if sum of forecast result is negative, then forecase all zero
  if (sum(forecast_arima$mean)<0) {
    ts.out <- numeric(12)
    } 
  else {
    ts.out <- forecast_arima$mean 
    }
  return(ts.out)
  }

forecase_result_arima<- data.frame(""date""=c(201801:201812))
for (i in 1:882){
  SIG.ts <- ts(SIG.data[,i+1],start = c(2014,1), freq = 12)
  if(forecast_zero(SIG.ts) == TRUE){
    forecase_result_arima[i+1]=numeric(12)
    } else { 
      forecase_result_arima[i+1]<- arima_forecast(ts.in=first_nonzero(SIG.ts))
      }
  colnames(forecase_result_arima)[i+1] <- colnames(SIG.data)[i+1]
  }

result_export_arima<- data.frame(cbind(colnames(forecase_result_arima),t(forecase_result_arima)))
write.xlsx(result_export_arima, ""sig_forecase_arima_3y.xlsx"")














































