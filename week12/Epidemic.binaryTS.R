library(readxl)
library(dplyr)
library(caret)

Epidemic <- read_excel("PowederyMildewEpidemic.xlsx")
Epidemic<-Epidemic%>%
  mutate(Outbreak = ifelse(Outbreak == "No",0,1)) %>%
  mutate(lag1=lag(Outbreak,1))
#Q4
Actual<-Epidemic$Outbreak[8:11]
NaiveF<-Epidemic$lag1[8:11]
NaiveF
Epidemic%>%select(Year,lag1)%>%.[12,]
confusionMatrix(NaiveF,Actual)

#Q5|6
#define object
train.df<-Epidemic[Epidemic$Year<=1994,] 
valid.df<-Epidemic[Epidemic$Year>1994,] 
lr.pred<-list()
for(i in 1:length(valid.df)){
  #define object
  train.df<-Epidemic[1:(8+i-1),]
  print(valid.df[i,1])
  #fit
  lr<-glm(Outbreak~`Max temp`+`Rel humidity`,data=train.df,family ='binomial')
  #forecast
  lr.pred_i<-predict(lr,valid.df[i,c(3,4)],type = "response")
  print(lr.pred_i)
  lr.pred<-append(lr.pred,lr.pred_i)
  
}
Q7
#accuracy
confusionMatrix(if_else(unlist(lr.pred)[1:4]>0.5,1,0),valid.df$Outbreak)                  
