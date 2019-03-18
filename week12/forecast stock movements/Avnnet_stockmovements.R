library(xts)
library(janitor)
library(forecast)
library(caret)
stock.data <- read.csv("Case-StockMovementsVar74.csv")
stock.xts <- xts(stock.data, order.by = excel_numeric_to_date(stock.data$Timestamp, include_time = TRUE))

# Difference predictors with lag 12
v74.open.diff <- diff(stock.xts$Variable74OPEN, lag = 12, na.pad=TRUE)
v74.last.diff <- diff(stock.xts$Variable74LAST_PRICE, lag = 12, na.pad=TRUE)
v74.high.diff <- diff(stock.xts$Variable74HIGH, lag = 12, na.pad=TRUE)
v74.low.diff <- diff(stock.xts$Variable74LOW, lag = 12, na.pad=TRUE)

# Take lag 13 of Target
target.lagged <- lag(stock.xts$TargetVariable, 13, na.pad =TRUE)

# Combine transformed predictors and target
new.data <- cbind.xts(stock.xts[,3:6],v74.high.diff,v74.last.diff,v74.low.diff,v74.open.diff,target.lagged)
new.data <- na.omit(new.data)

length(new.data)

# Partition into training and validation (2539 periods)
nvalid <- 2539
ntrain <- dim(new.data)[1]-nvalid
train <- new.data[1:ntrain, ]
valid <- new.data[(ntrain+1):(ntrain+nvalid), ]

# Neural net: avNNet = Neural Networks Using Model Averaging
nn.model <- avNNet(TargetVariable ~., data = train, repeats = 5, size=10)
nn.fit <- predict(nn.model, newdata = train)
confusionMatrix(as.factor(ifelse(nn.fit>0.5,1,0)), as.factor(train$TargetVariable))

nn.forecast <- predict(nn.model, newdata = valid)
confusionMatrix(as.factor(ifelse(nn.forecast>0.5,1,0)),as.factor(valid$TargetVariable))
