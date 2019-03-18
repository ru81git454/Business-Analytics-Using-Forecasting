library(forecast)
library(ggplot2)
library(lubridate)

data <- read.csv("SIG_forecast.csv")
ann <- data[2]
ana <- data[2]
anm <- data[2]
aan <- data[2]
aaa <- data[2]
aam <- data[2]
amn <- data[2]
ama <- data[2]
amm <- data[2]
mnn <- data[2]
mna <- data[2]
mnm <- data[2]
man <- data[2]
maa <- data[2]
mam <- data[2]
mmn <- data[2]
mma <- data[2]
mmm <- data[2]
exp <- data[2]
aadn <- data[2]
madn <- data[2]
leftover <- data[2]
hwin$method
for(i in 3:531){
  package.ts <-ts(data[i], start = c(2014,1), freq =12)
  valid.ts <- subset(package.ts, start = 37)
  train.ts <- subset(package.ts, end = 36)
  
  hwin <- ets(train.ts)
   if (hwin$method == "ETS(A,N,N)"){
     ann <- cbind(ann, data[i])
   } else if (hwin$method == "ETS(A,N,A)"){
     ana <- cbind(ana, data[i])
   } else if (hwin$method == "ETS(A,N,M)"){
     anm <- cbind(anm, data[i])
   } else if (hwin$method == "ETS(A,A,N)"){
     aan <- cbind(aan, data[i])
   } else if (hwin$method == "ETS(A,A,A)"){
     aaa <- cbind(aaa, data[i])
   } else if (hwin$method == "ETS(A,A,M)"){
     aam <- cbind(aam, data[i])
   } else if (hwin$method == "ETS(A,M,N)"){
     amn <- cbind(amn, data[i])
   } else if (hwin$method == "ETS(A,M,A)"){
     ama <- cbind(ama, data[i])
   } else if (hwin$method == "ETS(A,M,M)"){
     amm <- cbind(amm, data[i])
   } else if (hwin$method == "ETS(M,N,N)"){
     mnn <- cbind(mnn, data[i])
   } else if (hwin$method == "ETS(M,N,A)"){
     mna <- cbind(mna, data[i])
   } else if (hwin$method == "ETS(M,N,M)"){
     mnm <- cbind(mnm, data[i])
   } else if (hwin$method == "ETS(M,A,N)"){
     man <- cbind(man, data[i])
   } else if (hwin$method == "ETS(M,A,A)"){
     maa <- cbind(maa, data[i])
   } else if (hwin$method == "ETS(M,A,M)"){
     mam <- cbind(mam, data[i])
   } else if (hwin$method == "ETS(M,M,N)"){
     mmn <- cbind(mmn, data[i])
   } else if (hwin$method == "ETS(M,M,A)"){
     mma <- cbind(mma, data[i])
   } else if (hwin$method == "ETS(M,M,M)"){
     mmm <- cbind(mmm, data[i])
   } else if (hwin$method == "Simple exponential smoothing"){
     exp<- cbind(exp, data[i])
   } else if (hwin$method == "ETS(M,Ad,N)"){
     madn <- cbind(madn, data[i])
   } else if (hwin$method == "ETS(A,Ad,N)"){
     aadn <- cbind(aadn, data[i])
   } else {
     leftover <- cbind(leftover, data[i])
   }
}
