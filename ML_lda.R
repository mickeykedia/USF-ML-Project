
library(caret)

####################Linear Discriminant Analysis################
LDA_caret = function(X_train, Y_train, X_test, Y_test){
  trCtl=trainControl(method='repeatedcv',number=4)
  model = train(X_train, Y_train,'lda',trControl=trCtl)
  pred <- predict(model$finalModel, newdata = as.data.frame(X_test))
  pr <- prediction(as.numeric(pred$class), as.numeric(Y_test))
  
  return(model, pr)
}

checkNormality  =function(tarin){
  n <- ncol(train)
  str <- vector()
  for(i in 2:n){
    predictor <- train[,i]
    sh.out <- shapiro.test(predictor)
    if (sh.out$p.value > 0.05){
      outputStr <- paste0(names(train)[i], " is normally distributed.")  
    } else{
      outputStr <- paste0(names(train)[i], " is not normally distributed.")
    }
    str[i-1] <- outputStr
  }
  return(str)
}


LDA = function(train, test){
  #####Check Normality
  outnormal <- checkNormality(train)
  df <- data.frame(train)
  # change the name of response to Y
  names(df)[1]<-c('Y')
  coln = colnames(df)
  lda.fit <- lda(Y~., data = df, na.omit =TRUE)
  
  dfTest <- data.frame(test)
  names(dfTest)[1]<-c('Y')
  lda.pred <- predict(lda.fit, dfTest$Y)
  names(lda.pred)
  
  #evaluate how the method performed
  lda.class <- lda.pred$class
  confusion <- table(lda.class, dfTest$Y)
  #model the accuracy
  accuracy <- mean(lda.class == dfTest$Y)
  
  #the posterior probability
  posteriorProb <- lda.pred$posterior
  return(lda.fit, lda.pred, accuracy, confusion)
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



