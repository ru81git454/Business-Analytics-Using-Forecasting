library(dplyr)

SIG.df<-read.csv('SIG_adjusted_T.csv',header=FALSE)
SIG.df.T<-t(SIG.df)
SIG.df.T<-as.data.frame(SIG.df.T)
colnames(SIG.df.T) <- unlist(SIG.df.T[1,])
SIG.df.T<-SIG.df.T[,-c(1,3:8)]
SIG.df.T<-SIG.df.T[-1,]
SIG.df.T[,2:49]<-sapply(SIG.df.T[,2:49],as.numeric)
#group by customer
customer.group<-SIG.df.T %>%
  group_by(Customer)  %>%
  summarise_all(funs(sum))
  
  
  
write.csv(customer.group,'customer_group.csv')

