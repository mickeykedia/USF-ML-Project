
#Normalization
# Divide to test and tarin



library(MASS)
library("caret")


# 
# train <- (Year < 2005)
# Smarket.2005 <- Smarket[!train, ]
# Direction.2005 <- Direction[!train]
# Smarket.train <- Smarket[train, ]
# Smarket.Y <- Direction[train]
# trDF <- data.frame(Smarket.Y, Smarket.train)
# tsDF <- data.frame(Direction.2005, Smarket.2005)
# train <- data.frame(iris$Species[1:100], iris$Sepal.Length[1:100], iris$Sepal.Width[1:100], iris$Petal.Length[1:100], iris$Petal.Width[1:100])
# test <- data.frame(iris$Species[101:150], iris$Sepal.Length[101:150], iris$Sepal.Width[101:150], iris$Petal.Length[101:150], iris$Petal.Width[101:150])

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
st <- CheckNormality(train)
# do you wnat to do lDA after non normality
outputLDA <- LDA(train, test)




nbClassifier = function(tr, ts){

  xTrain = tr[,-1]
  yTrain = tr[,1]
  
  xTest = ts[,-1]
  yTest = ts[,1]
  
  model = train(xTrain,yTrain,'nb',trControl=trainControl(method='cv',number=10))
  
  k <- predict(model$finalModel,xTest)
  tb <- table(k$class, yTest)
  # prop.table(table(predict(model$finalModel,xTest)$class,yTest))
  return(model, tb)
}


# nbClassifier(trDF, tsDF)


  


