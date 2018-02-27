# Create a tuning grid
tune_grid_linear <- expand.grid(C = seq(1, 5, 1))

# Build the linear SVM model
start = proc.time()
svm_fit_linear = train(Response ~ ., data = train, 
                 method = "svmLinear",
                 preProc = c("center","scale"),
                 trControl = train_control,
                 tuneGrid = tune_grid_linear)
svm_fit_linear
proc.time() - start

# Get the model metrics of the test set
start = proc.time()
test_pred = predict(svm_fit_linear, test)
proc.time() - start

confMat_lin <- table(test$Response, test_pred)
a2 = sum(diag(confMat_lin))/sum(confMat_lin)

s2 = sqrt(sum((as.numeric(test_pred)-1 - mean(as.numeric(test_pred)-1))^2)/(nrow(test) - 1))

# 300.236 -> 60.1 sec