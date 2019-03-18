## Load libraries

library(forecast)
library(ggplot2)
library(lubridate)
library(stringr)


## Read data from CSV and create a dataframe

# bicup2006.csv has data on demand every 15-minutes for 3 weeks
SIG_df<-read.csv("/Users/pei/Documents/📔BAFT_2018/Project/SIG Europe data.csv",header = TRUE)

# check to see it loaded properly:
head(SIG_df)


## Manipulate the dataframe (useful for forecasting tasks) 
# e.g. create timestamp column, split series into multiple sub-series, change aggregation level
# Combine the DATE and TIME columns into single DATETIME recognized by R (using lubridate package):
SIG_df$Customer = paste(str_extract(SIG_df$Link..Cust...PH.,"^[0-9]+"))
SIG_df$MarketSegment = paste(str_extract(SIG_df$Link..Cust...PH.,'[A-Z]{1}$'))
SIG_df$temp = paste(str_extract(SIG_df$Link..Cust...PH.,'[0-9]+[A-Z]{1}$'))
SIG_df$PackageVolume = paste(substring(SIG_df$temp,4,6))
SIG_df$PackageFormat = paste(substring(SIG_df$temp,1,3))
SIG_df1 <- select( SIG_df, c('Customer','PackageFormat','PackageVolume','MarketSegment'))

SIG_total<-cbind(SIG_df1,SIG_df[,1:51])
final_df <- as.data.frame(t(SIG_total))


write.csv(SIG_total, file="SIGEuropedataadjusted.csv")



