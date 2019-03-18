library(readr)
## reading data

SIG.information<-read_csv("SIG Europe data.csv")[,(1:3)]
SIG.data<-t(read.csv("SIG Europe data.csv",header = FALSE)[,-(1:3)])
## take the two first character of the country name
SIG.information$Country_two<-substr(SIG.information$Country, start = 1, stop = 2)
## Combining customer codes and Country names
SIG.information$Column.name<-paste(SIG.information$`Link (Cust + PH)`, SIG.information$Country_two, sep="")
## use that created column as column name for SIG.data
colnames(SIG.data)<-c("date",SIG.information$Column.name)

write_csv(as.data.frame(SIG.data),'SIG data.csv')
