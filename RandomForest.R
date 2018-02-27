library(randomForest)

# Build the random forest model using the train set
start = proc.time()
rf = randomForest(Response ~., data = train)
proc.time() - start

# Predict the results and calculate the accuracy/sd of the test set.
pred_rf = predict(rf, test)
confMat_rf <- table(test$Response, predict(rf, test))
a_rf = sum(diag(confMat_rf))/sum(confMat_rf)
s_rf = sqrt(sum((as.numeric(pred_rf)-1 - mean(as.numeric(pred_rf)-1))^2)/(nrow(test) - 1))
confusionMatrix(confMat_rf)

varImpPlot(rf)
