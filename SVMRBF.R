# Create a tuning grid
tune_grid <- expand.grid(sigma = c(.01, .015, 0.2),
                         C = seq(1, 3, 1))

# Build the SVM model
start = proc.time()
svm_fit1 = train(Response ~ ., data = train, 
                 method = "svmRadial",
                 preProc = c("center","scale"),
                 trControl = train_control,
                 tuneGrid = tune_grid)
svm_fit1
proc.time() - start

# Get the accuracy and sd metrics
test_pred_1 = predict(svm_fit1, test)
confMat_1 <- table(test$Response, test_pred_1)
a1 = sum(diag(confMat_1))/sum(confMat_1)
confusionMatrix(confMat_1)

s1 = sqrt(sum((as.numeric(test_pred_1)-1 - mean(as.numeric(test_pred_1)-1))^2)/(nrow(test) - 1))

