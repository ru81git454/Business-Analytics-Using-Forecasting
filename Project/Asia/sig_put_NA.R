setwd("C:/Users/Lynn/Desktop/BAFT/SIG project/")
library(readxl)
library(forecast)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(zoo)
sig_pivot <- read.csv("APS_clean_version.csv",
                      header = TRUE,
                      row.names = 1)

#########data processing########
ts_data<-function(df){
  SIG_data<<-df %>%
    filter(Country=='Thailand')%>%
    select(-Country) %>%
    t(.) 
  nams<<- df%>%
    add_rownames('ID')%>%
    filter(Country=='Thailand')%>%
    select(ID) 
  data_w_nams<<-rbind(t(nams),SIG_data)
  sig_actual<<-as.data.frame(SIG_data)
  colnames(sig_actual) <<- nams$ID
}
ts_data(sig_pivot)

###########NA###############
put_NA = function(ts){
  for(i in 1:length(ts)){
    if(ts[i] == 0){
      ts[i]=NA}
    else{
      break}
  }
  return(ts)
}

sig_actual <- as.data.frame(sapply(sig_actual,put_NA))


# Those without 2 years full data.
drop_data = function(x){
  time_len <- length(x[,1])
  count_na = function(x){
    time_len - sum(is.na(x))
  }
  
  series_len = sapply(x,count_na)
  sig_cannot_pred <<- x[,which(series_len < 24)]
  sig_above_2years <- x[,which(series_len >= 24)]
  
  # Those last 2 years are all zero
  last_2years_zero = function(x){
    sum(x[(time_len-24+1) : time_len],na.rm = T)
  }
  
  series_drop <- sapply(sig_above_2years,last_2years_zero)
  sig_pred_actual <<- sig_above_2years[,which(series_drop != 0)]
  sig_drop <<- sig_above_2years[,which(series_drop == 0)]
}
drop_data(sig_actual)

