results = resamples(list(rf = rf, rbf = svm_fit1, linear = svm_fit_linear, poly = svm_fit_poly))
summary(results)
dotplot(results, xlim = c(0.65, 0.8))

modelCor(results)

varImp(svm_fit1, scale = FALSE)

models = c(svm_fit1, svm_fit_linear)

clist = caretStack(models)

#algorithms = c('svmRadial', 'svmLinear', 'svmPoly')
#models <- caretList(as.factor(CarInsurance)~., data=train, trControl=train_control, methodList=algorithms)

tune_list = list(
  caretModelSpec(method="svmRadial", tuneGrid=expand.grid(sigma=0.01, C=3)),
  caretModelSpec(method="glm"),
  caretModelSpec(method="rpart")
)

stacking = caretList(Response ~., data = train,
                     trControl = train_control,
                     tuneList = tune_list)

summary(resamples(stacking))

stack_glm = caretStack(stacking, method = "glm", metric = "Accuracy",
                       trControl = trainControl(method="cv",
                                                number=10,
                                                savePredictions="final",
                                                classProbs=TRUE)
                       )

# test$rbf = predict(svm_fit1, test, type = 'prob')
# test$lin = predict(svm_fit_linear, test, type = 'prob')
# test$poly = predict(svm_fit_poly, test, type = 'prob')
# 
# test$pred_avg = ((test$rbf$Yes*1) + test$lin$Yes*0 + test$poly$Yes*0)
# test$prediction = ifelse(test$pred_avg > 0.5, "Yes", "No")

#test$vote_rbf = ifelse(test$rbf == 'Yes', 1, 0)
#test$vote_lin = ifelse(test$lin == 'Yes', 1, 0)
#test$vote_poly = ifelse(test$poly == 'Yes', 1, 0)

#test$vote = test$vote_rbf + test$vote_lin + test$vote_poly
#test$prediction = ifelse(test$vote > 1, "Yes", "No")

confMat <- table(test$Response, predict(stack_glm, test))
sum(diag(confMat))/sum(confMat)

confMat <- table(test$Response, predict(svm_fit1, test))
sum(diag(confMat))/sum(confMat)

confMat <- table(test$Response, test$lin)
sum(diag(confMat))/sum(confMat)

confMat <- table(test$Response, test$poly)
sum(diag(confMat))/sum(confMat)
