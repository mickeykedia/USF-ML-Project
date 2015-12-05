source("Skeleton.R")


####################################



#' Parameter Evaluation for data
#' 
#' This function performs a basic parameter evaluation for the parameters of a classification object. 
#'
#'
#' @param The data frame with the first column as the categorical variable which has to be predicted and the rest of the columns as predictors
#' @return 
classification.param.evaluation(data){
  
  
  
}

#' Classification of the data
#' 
#'
#'
#' @param The data frame with the first column as the categorical variable which has to be predicted and the rest of the columns as predictors. If the first column is not the binary class then that must be specified in the outcome.col setting
#' @param outcome.col Explicitly sending a column name to be used as the binary class for which classifier is to be built. If this isn't supplied it is assumed that the first column is the binary class. 
#' @param classifier The classification method to use. This can be one of "all", "knn", "nb", ... 
#' @param predictors a boolean list of predictors to be used for the classifiers. If none is supplied then all predictors are used. 
#' @param k The tuning parameter for the k-means algorithm. If k is not supplied then the classifier will perform Cross validation to choose appropriate k
#' @return 
classification <- function(data, outcome.col, classifier, predictors, k){
  
  data <- removeNA(data)
  
  train.index <- sample(nrow(data), round(nrow(data)*0.8))
  test.index <- -train.index
  
  train <- data[train.index,]
  test <- data[test.index,]
  
  if(missing(outcome.col)){
    Y_train <- train[1]
    Y_test <- test[1]
    X_train <- train[-1]
    X_test <- test[-1]
  }else {
    select.Y <- names(train)==outcome.col
    Y_train <- train[select.Y]
    Y_test <- test[select.Y]
    
    X_train <- train[!select.Y]
    X_test <- test[!select.Y]
  }
  
  #X_train <- convertCategoricalToDummy(data)
  # There is a problem with using this function here. It changes the names of the categorical 
  # variable (obviously) and so user supplied names can't be used simply. 
  # One way out is to provide this as a helper function to the user instead of 
  # preprocessing the data for the user. 
  
  
  
  
  
 ####################### CLASSIFICATION ZONE ##################### 
  
  if(classifier=="knn"){
    o <- k.nearest.neighbour(Y_train = Y_train, X_train = X_train, Y_test = Y_test, X_test = X_test)
    return(o)
  }
}







################ TESTING #######################


titanic <- read.csv("~/Dropbox/ML_project/Datasets/Titanic (for development)/titanic3.csv", header = TRUE)
# removing name 
titanic <- titanic[-3]

# removing home destination
titanic <- titanic[-13]
names(titanic)

# removing cabin
titanic <- titanic[-9]

# removing embarked
titanic <- titanic[-9]

# removing body
titanic <- titanic[-10]


p = 0.8 


X_train <- data.frame()
X_test <- data.frame()

for(i in seq(nrow(titanic))){
  if(runif(1)>0.8){
    X_test <- rbind(X_test, titanic[i,])
  }else{
    X_train <- rbind(X_train, titanic[i,])
  }
}

Y_test <- X_test[2]
Y_train <- X_train[2]
X_train <- X_train[-2]
X_test <- X_test[-2]

#X_train <- convertCategoricalToDummy(X_train)
#X_test <- convertCategoricalToDummy(X_test)

## Giving me an error ! 



o <- k.nearest.neighbour(Y_train, X_train, Y_test, X_test)

res <- classifier.metrics(o[1], print.flag = TRUE)

#### Prediction object - can be used to draw curves etc ##### 
per <- performance(o[[1]], measure = "acc")
plot(per)

roc.1 = performance(o[[1]], measure = 'tpr', x.measure = 'fpr')
per <- performance(o[[1]], measure = "fpr", x.measure = "fnr")
plot(per)
per <- performance(o[[1]], measure = "auc")
plot(per)



###########################
o <- classification(data = GermanCredit, outcome.col = "Class.Good", classifier = "knn")

roc.1 = performance(o[[1]], measure = 'tpr', x.measure = 'fpr')
plot(roc.1)

roc.1


names(o)


classifier.metrics(o,print.flag = TRUE)


# Defining Plot and summary relationship ! 
class(o) <- c("list", "knn")

plot <- function(o){
  UseMethod("plot", o)
}

plot.knn <- function(o){
  per <- performance(o[[1]], measure = "fpr", x.measure = "fnr")
  plot(per)
}

plot(o)


