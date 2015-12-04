
library(caret)

####################Linear Discriminant Analysis################
LDA = function(X_train, Y_train, X_test, Y_test){
  trCtl=trainControl(method='repeatedcv',number=4)
  model = train(X_train, Y_train,'lda',trControl=trCtl)
  pred <- predict(model$finalModel, newdata = as.data.frame(X_test))
  pr <- prediction(as.numeric(pred$class), as.numeric(Y_test))
  
  return(model, pr)
}



#######NAIVE BAYES#############
nbClassifier = function(X_train, Y_train, X_test, Y_test){
  trCtl=trainControl(method='repeatedcv',number=4)
  model = train(X_train, Y_train,'nb',trControl=trCtl)
  pred <- predict(model$finalModel,X_test)
  pr <- prediction(as.numeric(pred$class), Y_test)
  
  return(model, pr)
}



############Quadratic discriminant Analysis################################
QDA = function(X_train, Y_train, X_test, Y_test){
  trCtl=trainControl(method='repeatedcv',number=4)
  model = train(X_train, Y_train,'qda',trControl=trCtl)
  pred <- predict(model$finalModel, newdata = as.data.frame(X_test))
  pr <- prediction(as.numeric(pred$class), as.numeric(Y_test))
  
  return(model, pr)
}



