
#Normalization
# Divide to test and tarin

install.packages("ROCR")
library(ROCR)
library(MASS, quietly = TRUE)
#demo(ROCR)

# put y as 1 st column in the dataframe
# remaining all are x - predictors
train <- data.frame(iris$Species[1:100], iris$Sepal.Length[1:100], iris$Sepal.Width[1:100], iris$Petal.Length[1:100], iris$Petal.Width[1:100])
test <- data.frame(iris$Species[101:150], iris$Sepal.Length[101:150], iris$Sepal.Width[101:150], iris$Petal.Length[101:150], iris$Petal.Width[101:150])

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
  lda.fit <- lda(df$Y~., data = df, na.omit =TRUE)
  #look at a summary of the fit
  # do we want to plot the model here 
  #lda.fit
  #plot(lda.fit)
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





  


