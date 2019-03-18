logistic.model <- glm(TargetVariable ~ . , family="binomial", data = train)
summary(logistic.model)
confusionMatrix(as.factor(ifelse(logistic.model$fitted.values>0.5,1,0)), as.factor(train$TargetVariable))

logistic.forecast <- predict(logistic.model, newdata = valid, type = "response")
confusionMatrix(as.factor(ifelse(logistic.forecast>0.5,1,0)), as.factor(valid$TargetVariable))
