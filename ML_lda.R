
library(caret)
####################Linear Discriminant Analysis################
LDA = function(xTrain, yTrain, xTest, yTest){
  model = train(xTrain, yTrain,'lda',trControl=trainControl(method='cv',number=10))
  
  k <- predict(model$finalModel, xTest)
  tb <- table(k$class, yTest)
  # prop.table(table(predict(model$finalModel,xTest)$class,yTest))
  return(model, tb)
}



#######NAIVE BAYES#############
nbClassifier = function(trainX, trainY, testX, testY){

  xTrain = trainX
  yTrain = trainY
  
  xTest = testX
  yTest = testY
  
  model = train(xTrain,yTrain,'nb',trControl=trainControl(method='cv',number=10))
  
  k <- predict(model$finalModel,xTest)
  tb <- table(k$class, yTest)
  # prop.table(table(predict(model$finalModel,xTest)$class,yTest))
  return(model, tb)
}




############Quadratic discriminant analysis################################
QDA = function(xTrain, yTrain, xTest, yTest){
  model = train(xTrain, yTrain,'qda',trControl=trainControl(method='cv',number=10))
  k <- predict(model$finalModel, xTest)
  tb <- table(k$class, yTest)
  # prop.table(table(predict(model$finalModel,xTest)$class,yTest))
  return(model, tb)
  
}

  


