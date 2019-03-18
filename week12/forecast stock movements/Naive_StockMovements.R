library(caret)

data<- read.csv("StockMovementTrainingData.csv")

nvalid <- 2539
ntrain <- dim(data)[1]-nvalid
train <- data[1:ntrain, ]
valid <- data[(ntrain+1):(ntrain+nvalid), ]

mean(train$TargetVariable)
valid$naive=1
valid$naive
confusionMatrix(valid$naive,valid$TargetVariable)
unique(valid$TargetVariable)
