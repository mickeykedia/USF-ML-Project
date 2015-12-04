# MSAN621 - Machine Learning
# Final Project: Classification

library(corrplot)

# Functions to add to skeleton:

identifyCategoricalContinuousVars <- function(X){
  s = lapply(X, class)
  # Returns categorical as TRUE
  return(s == "factor")
}

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



# perhaps we should use a tryCatch when calling the function
tryCatch(significantPredictors(df_numeric[,2:5], df_numeric[,1, drop=FALSE]),
         finally = print('Review input'))


predictorMeans <- function(X){
  numeric_cols = sapply(X, is.numeric)
  X_numeric = X[,numeric_cols]
  var_mean = lapply(X_numeric, mean, na.rm = TRUE)
  return(unlist(var_sd))
}

predictorStandardDeviations <- function(X){
  numeric_cols = sapply(X, is.numeric)
  X_numeric = X[,numeric_cols]
  var_sd = lapply(X_numeric, sd, na.rm = TRUE)
  return(unlist(var_sd))
}

removeNA <- function(X){
  return(X[complete.cases(X),])
}


convertCategoricalToDummy <- function(X){
  to_del = c()
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






# (0) DATA DESCRIPTION #######################################################
Y = df[,2]
X = df[,-2]
data.description <- function(X, Y = null){
  
  # Check if Y is first column of X, and bind in single dataframe
  if (!is.null(Y)){
    df = cbind(Y,X) 
  }else{
    df = X
  }
  
  # Print n, p
  n = now(df)
  p = ncol(df) - 1
  cat('\n##### DATA DESCRIPTION #####',
    '\nNumber of observations: ', n, '\nNumber of variables: ',p)
  
  # Number of NA values
  cat('\nNumber of missing values: ', sum(is.na(df)))
  cat('\nNumber of complete observations: ', sum(complete.cases(df)))
  
  # Type of variables
  s = lapply(df, class)
  cat('Number of continuous variables:', sum(s == "numeric") + sum(s == "integer"))
  cat('Number of categorical variables:', sum(s == "factor"))
  cat('Number of binary variables:', sum(lapply(lapply(df, unique),length) == 2))
  
  # Number of significant variables in logistic regression
  X2 = data.matrix(df[,2:(p+1)])
  Y2 = data.matrix(df[,1,drop=FALSE])
  glm.fit <- glm(Y2 ~ X2, family = binomial)
  s = summary(glm.fit)
  p_vals = s$coefficients[-1,4]
  cat('Number of statistically significant variables (in logistic regression):',
      sum(p_vals < 0.05))
  
  # Comparison of means and sds
  numeric_cols = sapply(df, is.numeric)
  df_numeric = df[,numeric_cols]
  var_mean = lapply(df_numeric, mean, na.rm = TRUE)
  var_sd = lapply(df_numeric, sd, na.rm = TRUE)
  df_msd = data.frame('Mean' = unlist(var_mean), 
                      'SD' = unlist(var_sd))
  cat('\nMean and standard deviation of numeric variables:')
  print.data.frame(df_msd, digits = 3, right = FALSE)
  
  # Check potentially colinear variables
  par(mfrow = c(1,1))
  df_cor = data.frame(cor(na.omit(df_numeric[,2:ncol(df_numeric)])))
  #corrplot.mixed(cor(na.omit(df_numeric[,2:ncol(df_numeric)])), 
  #      upper='circle', tl.col='black')
  
  
} 




