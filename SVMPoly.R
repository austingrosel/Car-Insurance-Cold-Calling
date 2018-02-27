# Tuning grid for polynomial SVM's
tune_grid_poly <- expand.grid(degree = c(2, 3),
                         scale = c(0.1, 0.3),
                         C = seq(1, 2, 1))

# Build the SVM polynomial kernel
start = proc.time()
svm_fit_poly = train(Response ~ ., data = train, 
                     method = "svmPoly",
                     preProc = c("center","scale"),
                     trControl = train_control,
                     tuneGrid = tune_grid_poly)
svm_fit_poly
proc.time() - start

# Get the metrics for the polynomial kernel
start = proc.time()
test_pred = predict(svm_fit_poly, test)
proc.time() - start

confMat_poly <- table(test$Response, test_pred)
a3 = sum(diag(confMat_poly))/sum(confMat_poly)
s3 = sqrt(sum((as.numeric(test_pred)-1 - mean(as.numeric(test_pred)-1))^2)/(nrow(test) - 1))

#1025.917 -> 128.25
