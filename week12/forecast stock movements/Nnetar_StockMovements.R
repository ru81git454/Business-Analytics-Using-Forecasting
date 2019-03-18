# Partition into training and validation (2539 periods)
nvalid <- 2539
ntrain <- dim(new.data)[1]-nvalid
train <- new.data[1:ntrain, ]
valid <- new.data[(ntrain+1):(ntrain+nvalid), ]

# Neural net: nnetar
nn1 <- nnetar(train$TargetVariable, xreg=train[,1:8], repeats = 5, size=10)
confusionMatrix(as.factor(ifelse(nn1$fitted>0.5,1,0)), as.factor(train$TargetVariable))

nn1.forecast <- predict(nn1, newdata = valid, xreg=valid[,1:8])
confusionMatrix(as.factor(ifelse(nn1.forecast$mean>0.5,1,0)), as.factor(valid$TargetVariable))
