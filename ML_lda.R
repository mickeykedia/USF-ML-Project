
library(caret)
library(MASS)
library(ISLR)
####################Linear Discriminant Analysis################
LDA_caret = function(X_train, Y_train, X_test, Y_test){
  trCtl=trainControl(method='repeatedcv',number=4)
  model = train(X_train, Y_train,'lda',trControl=trCtl)
  pred <- predict(model$finalModel, newdata = as.data.frame(X_test))
  pr <- prediction(as.numeric(pred$class), as.numeric(Y_test))
  
  return(model, pr)
}

LDA = function(Y_train, X_train, Y_test, X_test){
  df <- data.frame(Y_train, X_train)
  # change the name of response to Y
  names(df)[1]<-c('Y')
  lda.fit <- lda(Y~., data = df, na.omit =TRUE)
  
  dfTest <- data.frame(Y_test, X_test)
  names(dfTest)[1]<-c('Y')
  lda.pred <- predict(lda.fit, X_test)
  #evaluate how the method performed
  lda.class <- lda.pred$class
  pr <- prediction(as.numeric(lda.class), as.numeric(Y_test))
  output = c(pr, lda.fit)
  return(output)
#   confusion <- table(lda.class, dfTest$Y)
#   #model the accuracy
#   accuracy <- mean(lda.class == dfTest$Y)
#   
#   #the posterior probability
#   posteriorProb <- lda.pred$posterior
#   return(lda.fit, lda.pred, accuracy, confusion)
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



