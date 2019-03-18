library(readr)
library(dplyr)
##define object
SIG <- read_csv("SIG data.csv")
SIG_exclude<-SIG[which(apply(SIG[37:48,],2, sum)==0)]
colnames(SIG_exclude)<-substr(colnames(SIG_exclude),1,(nchar(colnames(SIG_exclude))-2))

SIG_forecast<-SIG[which(apply(SIG[37:48,],2, sum)!=0)]
#
SIG_forecast_1<-subset(SIG_forecast,select = c(1,2:135))
SIG_forecast_2<-subset(SIG_forecast,select = c(1,136:270))
SIG_forecast_3<-subset(SIG_forecast,select = c(1,271:405))
SIG_forecast_4<-subset(SIG_forecast,select = c(1,406:530))

##output part
write.csv(SIG_exclude,'SIG_exclude.csv')
##forecast part
write.csv(SIG_forecast_1,'SIG_forecast_1.csv')
write.csv(SIG_forecast_2,'SIG_forecast_2.csv')
write.csv(SIG_forecast_3,'SIG_forecast_3.csv')
write.csv(SIG_forecast_4,'SIG_forecast_4.csv')
write.csv(SIG_forecast,'SIG_forecast.csv')


