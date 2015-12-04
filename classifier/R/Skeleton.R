library(ade4, quietly = TRUE)
library(caret, quietly = TRUE)
library(ROCR, quietly = TRUE)
library(class, quietly = TRUE)




########## PARAM EVALUATION ##############
# Split vars as categorical and continuous
# Find Number of NA's 
# sum(is.na(df))
# List of significant vars according to logistic regression
# Plot means of each variable
# Plot Standard devs of each variable
# Plot PCA Scree plot for all variables
# List of highly collinear variables 
# Split data into train and test set 




#' Identify predictors which are categorical or otherwise
#' 
#' @param X The dataframe with columns corresponding to predictors and rows corresponding to observations
#' @return A named list of flags for categorical and continuous with names corresponding to variable names or column numbers
identifyCategoricalContinuousVars <- function(X){
  s = lapply(X, class)
  # Returns categorical as TRUE
  return(s == "factor")
}

#' Identify significant predictors from all given predictors
#' 
#' We use the backward propogation algorithm on logistic regression to get a list of significant predictors
#' 
#' @param X The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @param Y The dataframe with the given classification for every observation
#' @return A named list of booleans with the TRUE corresponding to significant predictors and FALSE corresponding to insignificant predictors
significantPredictors <- function(X, Y){
  # Fit initial regression
  X2 = data.matrix(X)
  glm.fit <- glm(Y ~ X2, family = binomial)
  s = summary(glm.fit)
  # Remove statistically insignificant one at a time
  repeat{
    p_vals = s$coefficients[-1,4]
    min_p = max(p_vals)
    if (min_p < 0.05){
      break
    }
    if (sum(is.na(p_vals))>0){
      ind = which(is.na(p_vals) == TRUE)[1]
    } else {
      ind = which(p_vals == min_p)  
    }
    X2 = X2[,-ind]
    glm.fit <- glm(Y ~ X2, family = binomial)
    s = summary(glm.fit)
  }
  output = colnames(X) %in% colnames(X2)
  names(output) = colnames(X)
  return(output)
}


#' List of means of continuous predictors
#' 
#' categorical predictors are ignored. 
#' 
#' @param X The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @return A named list of means of each continuous predictor in the dataframe
predictorMeans <- function(X){
  numeric_cols = sapply(X, is.numeric)
  X_numeric = X[,numeric_cols]
  var_mean = lapply(X_numeric, mean, na.rm = TRUE)
  return(unlist(var_sd))
  
}


#' List of standard deviations of continuous predictors
#' 
#' categorical predictors are ignored. 
#' 
#' @param X The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @return A named list of standard deviations of each continuous predictor in the dataframe
predictorStandardDeviations <- function(X){
  numeric_cols = sapply(X, is.numeric)
  X_numeric = X[,numeric_cols]
  var_sd = lapply(X_numeric, sd, na.rm = TRUE)
  return(unlist(var_sd))
  
}

#' Variance Explained by component
#' 
#' Gives a list of percentage variance explained by each PCA component. 
#' This list can then be used to create scree plots for these decompositions
#' @param X The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @return A named list of values representing percentage explained by each component. Names correspond to which component (PC1, PC2 etc)
predictorPCAVarianceExplained <- function(X){
  
}

#' Point out highly collinear variables 
#' 
#' 
#' @param X The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @return 
predictorCollinearity <- function(){
  
}

########## DATA PREPARATION #############
# Check for NA's and remove those rows
# Convert categorical variables to dummy variables
# Scale and Center data
  # Use inbuilt function

#' Remove rows which have NA
#' 
#' 
#' @param X The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @return A dataframe derived from X which does not contain observations (rows) which have NA's in any column for that row 
removeNA <- function(X){
  return(X[complete.cases(X),])
}

#' Convert categorical variables to dummy variables
#' 
#' Converts those rows which are factors to dummy variables coded as 0,1,2 .. k for k factors
#' 
#' @param X The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @return A dataframe derived from X which does not contain observations (rows) which have NA's in any column for that row 
convertCategoricalToDummy <- function(X){
  to_del = c()
  p <- ncol(X)
  for (i in 1:p){
    if (is.factor(X[,i])){
      X_temp = acm.disjonctif(X[,i, drop=FALSE])
      df = cbind(X, X_temp)
      to_del[length(to_del)+1] = i
      }
    }
  X = X[,-to_del]
  return(X)

}



########## ALL CLASSIFIERS ############## 
# KNN
# Naive Bayes
# Logistic Regression
# Linear Discriminant Analysis
# Quadratic Discriminant Analysis
# Decision Tree 

#' KNN classification
#' 
#' Just performs the KNN classification without any preparation for the data. 
#' 
#' @param X_train The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @param X_test The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @param Y_train 
#' @param Y_test 
#' @param k optional, if not supplied then Cross validation will be performed to choose k
#' @param nfolds Number of folds in the cross validation
#' @return fitted model and prediction object
k.nearest.neighbour <- function(Y_train, X_train, Y_test, X_test, k, nfolds = 4){
  # Perform cross validation
  ctrl <- trainControl(method = "repeatedcv", number = nfolds, savePredictions = TRUE)
  Y_train2  = as.factor(as.character(data.matrix(Y_train)))
  # If k is not specified, then tune with 10 "k"s
  if(missing(k)) {
    cv.fit <- train(Y_train2 ~ ., data = data.matrix(X_train), method="knn",
                    trControl = ctrl, tuneLength = 10)
    k = cv.fit$bestTune[[1]]
  }else{
    # Otherwise fit (or tune) with the specified value(s) of k
    cv.fit <- train(Y_train2 ~ ., data = data.matrix(X_train), method="knn",
                    trControl = ctrl, tuneGrid = data.frame(k))
  }
  knn.pred = predict(cv.fit$finalModel, newdata = as.data.frame(X_test), type = "class")
  pr <- prediction(as.numeric(as.character(knn.pred)), Y_test)
  
  # Return model and prediction objects
  output = c(pr, cv.fit$finalModel)
  return(output)
}

    

#' Naive Bayes Classifier
#' 
#' Just performs the Naive bayes classification without any preparation for the data. 
#' 
#' @param X_train The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @param X_test The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @param Y_train 
#' @param Y_test 
#' @return fitted model and prediction object
naive.bayes <- function(Y_train, X_train, Y_test, X_test){
  
}
#' Logistic Regression Classifier  
#'  
#' Runs logistic regression, runs cross-validation to find optimal coefficients and 
#' finds threshold probability that optimizes classification. Does not preprocessing on the dataset provided
#' @param X_train The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @param X_test The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @param Y_train 
#' @param Y_test
#' @param nfolds Number of folds in the cross validation  
#' @return fitted model and prediction object
logistic.regression <- function(Y_train, X_train, Y_test, X_test, nfolds = 4){
  # Copy the training set
  X_train2 = data.matrix(X_train)
  Y_train2 = data.matrix(Y_train)
  # Fit initial model
  glm.fit <- glm(Y_train2 ~ X_train2, family = binomial)
  s = summary(glm.fit)
  # Re-fit until retaining only statistically significant coefficients
  repeat{
    p_vals = s$coefficients[-1,4]
    min_p = max(p_vals)
    if (min_p < 0.05){
      break
    }
    if (sum(is.na(p_vals))>0){
      ind = which(is.na(p_vals) == TRUE)[1]
    } else {
      ind = which(p_vals == min_p)  
    }
    X_train2 = X_train2[,-ind]
    glm.fit <- glm(Y_train2 ~ X_train2, family = binomial)
    s = summary(glm.fit)
  }
  # Now run cross validation to find the optimal coefficients with those variables
  ctrl <- trainControl(method = "repeatedcv", number = nfolds, savePredictions = TRUE)
  Y_train2  = as.factor(as.character(Y_train2))
  cv.fit <- train(Y_train2 ~ ., data = X_train2, method="glm", family="binomial",
                  trControl = ctrl, tuneLength = 5)
  s = summary(cv.fit)
  cv.fit$finalModel
  glm.probs <- predict(cv.fit$finalModel, newdata = as.data.frame(X_train2), type = "response")
  d1 <- length(glm.probs)
  
  # Optimize threshold to classify 0/1 in the training set 
  lr_accuracy = c()
  cutoff = seq(0,1,0.01)
  for (i in cutoff){
    glm.pred.train <- rep(0, d1)  
    glm.pred.train[glm.probs > i] = 1
    accur = mean(glm.pred.train == Y_train)
    lr_accuracy = c(lr_accuracy, accur)
  }
  ind = which(lr_accuracy == max(lr_accuracy))
  # Predict in training set using the optimal threshold
  glm.pred.train <- rep(0, d1)  
  glm.pred.train[glm.probs > cutoff[ind]] = 1
  accur = mean(glm.pred.train == Y_train)
  # Predict in test set
  X_test2 = data.matrix(X_test[,colnames(X_train2)])
  glm.probs <- predict(cv.fit$finalModel, newdata = as.data.frame(X_test2), type = "response")
  d2 = length(glm.probs)
  glm.pred.test <- rep(0, d2)  
  glm.pred.test[glm.probs > cutoff[ind]] = 1
  pr <- prediction(glm.pred.test, Y_test)
  # Return model and prediction objects
  output = c(pr, cv.fit$finalModel)
  return(output)
  
}

#' Linear Discriminant Analysis Classifier 
#' 
#' Performs LDA classification for the data. The function expects X_train and X_test to only have continuous variables. 
#' The function doesn't check any assumptions (for normality or same covariance matrix for the two conditional probabilities)
#' 
#' @param X_train The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @param X_test The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @param Y_train 
#' @param Y_test 
#' @return fitted model and prediction object
linear.discriminant.analysis <- function(Y_train, X_train, Y_test, X_test){
  
}

#' Quadratic Discriminant Analysis Classifier 
#' 
#' Performs QDA classification for the data. The function expects X_train and X_test to only have continuous variables. 
#' The function doesn't check any assumptions (for normality)
#' 
#' @param X_train The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @param X_test The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @param Y_train 
#' @param Y_test 
#' @return fitted model and prediction object
quadratic.discriminant.analysis <- function(Y_train, X_train, Y_test, X_test){
  
}
#' Random Forest Classifier
#' 
#' Performs a Random forest classification for the data. How to choose the params depends on 
#' what options the function we are using provides us. We could ask the user to specify this or 
#' choose m as sqrt(p) and B as some reasonably large number related to the number of observations we have
#' 
#' @param X_train The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @param X_test The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @param Y_train 
#' @param Y_test 
#' @param B the number of trees to fit 
#' @param m the number of parameters that each tree will have 
#' @return fitted model and prediction object
random.forest <- function(Y_train, X_train, Y_test, X_test, B, m){
  
}


########## OUTPUT RELATED ###############
# Summary of prediction objects
# Provide list of model assumptions for each model specified
# Plot Accuracy/ROC/AUC/MSE/MSPE Curves

classifier.metrics <- function(pred.obj, print.flag = FALSE){
  "Return classifier statistics in the test set
  Input: Prediction object (ROCR)
  Output: A list with MSPE, accuracy, sensitivity and specificity"
  mspe = mspe = mean((slot(pred.obj[[1]], 'predictions')[[1]] - 
          as.numeric(as.character(slot(pred.obj[[1]], 'labels')[[1]])))^2)
  accuracy = slot(performance(pred.obj[[1]], "acc"), "y.values")[[1]][2]
  sensitivity = slot(performance(pred.obj[[1]], "sens"), "y.values")[[1]][2]
  specificity = slot(performance(pred.obj[[1]], "spec"), "y.values")[[1]][2]
  
  if (print.flag){
    cat('\n- Classifier metrics:\n   MSPE: ', mspe, '\n   Accuracy: ', 
    accuracy, '\n   Sensitivity: ', sensitivity, '\n   Specificity: ', 
    specificity)
  }
  return(c(mspe, accuracy, sensitivity, specificity))
}


