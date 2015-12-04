

########## PARAM EVALUATION ##############
# Split vars as categorical and continuous
# Find Number of NA's 
sum(is.na(df))
# List of significant vars according to logistic regression
# Plot means of each variable
# Plot Standard devs of each variable
# Plot PCA Scree plot for all variables
# List of highly collinear variables 




#' Identify predictors which are categorical or otherwise
#' 
#' @param X The dataframe with columns corresponding to predictors and rows corresponding to observations
#' @return A named list of flags for categorical and continuous with names corresponding to variable names or column numbers
identifyCategoricalContinuousVars <- function(X){
}

#' Identify significant predictors from all given predictors
#' 
#' We use the backward propogation algorithm on logistic regression to get a list of significant predictors
#' 
#' @param X The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @param Y The dataframe with the given classification for every observation
#' @return A named list of booleans with the TRUE corresponding to significant predictors and FALSE corresponding to insignificant predictors
significantPredictors <- function(X, Y){
  
}


#' List of means of continuous predictors
#' 
#' categorical predictors are ignored. 
#' 
#' @param X The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @return A named list of means of each continuous predictor in the dataframe
predictorMeans <- function(X){
  
}


#' List of standard deviations of continuous predictors
#' 
#' categorical predictors are ignored. 
#' 
#' @param X The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @return A named list of standard deviations of each continuous predictor in the dataframe
predictorStandardDeviations <- function(X){
  
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
  
}

#' Convert categorical variables to dummy variables
#' 
#' Converts those rows which are factors to dummy variables coded as 0,1,2 .. k for k factors
#' 
#' @param X The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @return A dataframe derived from X which does not contain observations (rows) which have NA's in any column for that row 
convertCategoricalToDummy <- function(X){

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
#' 
#' 
#' @param X_train The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @param X_test The dataframe with columns corresponding to predictors and rows corresponding to observations 
#' @param Y_train 
#' @param Y_test 
#' @param k optional, if not supplied then Cross validation will be performed to choose k
#' @return 
k.nearest.neighbour <- function(Y_train, X_train, Y_test, X_test, k){
  if(missing(k)) {
    # Perform cross validation to find best k 
  } 
    
}


########## OUTPUT RELATED ###############
# Summary of prediction objects
# Provide list of model assumptions for each model specified
# Plot Accuracy/ROC/AUC/MSE/MSPE Curves
