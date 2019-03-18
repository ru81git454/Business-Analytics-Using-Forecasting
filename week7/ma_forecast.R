library(forecast)
library(readr)
library(zoo)
data<-read_csv("SIG data1.csv", 
               col_types = cols(X1 = col_skip()))
#for saving forecasting value
fore_value <- data[37:48,1]+100

# model comparing
for (i in 2:length(data)){
  
  colname <- paste(colnames(data[i]))
  #define object
  package.ts <-ts(data[i], start = c(2014,1), freq =12)
  #moving average
  package.ma=rollmean(package.ts,k=2,align='right')
  last.ma<-tail(package.ma,12)
  last.ma.df<-as.data.frame(last.ma)
  colnames(last.ma.df)=colname
  fore_value<-cbind(fore_value,last.ma.df)
}
fore_value.t<-data.frame(date=colnames(fore_value),t(fore_value))

write_csv(fore_value.t,'fore_value.csv')


