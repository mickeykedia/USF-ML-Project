source("Skeleton.R")


####################################



#' Parameter Evaluation for data
#'
#' This function performs a basic parameter evaluation for the parameters of a classification object.
#'
#'
#' @param The data frame with the first column as the categorical variable which has to be predicted and the rest of the columns as predictors
#' @return
classification.param.evaluation <- function(data){

  cat('\n\n***************** PARAMETER EVALUATION **************************',
      '*****\n\n', sep = "")

  # (1) Identify categorical
  n = nrow(data)
  p = ncol(data)
  nonnum = identifyNonNumericVars(data)
  ncomplete = nrow(removeNA(data))
  cat('Total number of observations:       ', n, '\n',
      'Total number of complete cases:     ', ncomplete, '\n',
      'Total number of variables:          ', p, '\n',
      ' - Number of non-numeric variables: ', sum(nonnum), '\n',
      ' - Number of numeric variables:     ', p - sum(nonnum), '\n', sep = "")

  # (2) Mean and standard deviation of each predictor
  cat('\nMean of each predictor:\n')
  print(predictorMeans(data[,2:p]))
  cat('\nStandard deviation of each predictor:\n')
  print(predictorStandardDeviations(data[,2:p]))


  # (3) Which predictors are significant?
  data = data[,!nonnum]
  p = ncol(data)
  sig.pred = significantPredictors(data[,2:p],data[,1])
  names.sig = names(sig.pred[sig.pred == TRUE])
  cat('\nSignificant predictors in logistic regression:\n')
  cat(paste(names.sig[1:length(names.sig)-1], ' , '),
      names.sig[length(names.sig)])

  # (4) Collinearity
  cat('\n\nAnalyzing collinearity:\n')
  predictorCollinearity(data[,2:p], threshold = 0.8)

  # (5) Variance explained by each PCA
  x.pca = predictorPCAVarianceExplained(data[,2:p])
  cat('\n\nPrincipal Component Analysis (PCA): Variance explained\n')
  print(x.pca, digits = 3)
  names(x.pca) = 1:length(x.pca)
  .pardefault <- par(no.readonly = T)
  par(mfrow = c(1,2), oma = c(1,1,1,0), mar = c(5,2,4,2))
  barplot(x.pca, col = 'aliceblue', xlab = 'PC', ylab = '% Variance',
          main = 'Individual')
  plot(cumsum(x.pca), type='l', main= 'Cumulative',
       lwd = 2, col = 'blue', ylab = '% Variance', xlab = 'Number of PCs')
  abline(v = min(which(cumsum(x.pca) > 0.8)), col = 'skyblue', lwd = 2, lty =2)
  mtext(expression(bold('PCA: Variance explained')), outer = TRUE, cex = 1.2, line = -1)

  # Reset plotting parameters
  cat('\n\n')
  par(mfrow = c(1,1))
  par(.pardefault)
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
#' @param max.pred Number of params in each tree for the Random Forest classifier, defaults to
#' @return
classification <- function(data, outcome.col, classifier, predictors, k, max.pred){

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
    select.Y <- (names(train)==outcome.col)
    Y_train <- train[select.Y]
    Y_test <- test[select.Y]

    X_train <- train[!select.Y]
    X_test <- test[!select.Y]
  }
  rownames(X_train) <- NULL
  rownames(X_test) <- NULL
  rownames(Y_train) <- NULL
  rownames(Y_test) <- NULL

  #X_train <- convertCategoricalToDummy(data)
  # There is a problem with using this function here. It changes the names of the categorical
  # variable (obviously) and so user supplied names can't be used simply.
  # One way out is to provide this as a helper function to the user instead of
  # preprocessing the data for the user.



 ####################### CLASSIFICATION ZONE #####################

  if (classifier=="knn"){
    o <- k.nearest.neighbour(Y_train = Y_train, X_train = X_train, Y_test = Y_test, X_test = X_test)
    #class(o) <- list("classificationOutput", "knn")
    return(o)
  } else if (classifier == "nb"){
    o <- naive.bayes(Y_train = Y_train, X_train = X_train, Y_test = Y_test, X_test = X_test)

    return(o)
  } else if (classifier == "lr"){
    o <- logistic.regression(Y_train = Y_train, X_train = X_train, Y_test = Y_test, X_test = X_test)
    return(o)
  } else if (classifier == "lda"){
    ## Check for only continuous variables
    # categorical.vars <- identifyCategoricalContinuousVars(X_train)
    o <- linear.discriminant.analysis(Y_train = Y_train, X_train = X_train, Y_test = Y_test, X_test = X_test)
    return(o)
  } else if (classifier == "qda"){
    ## Check for only continuous variables
    # categorical.vars <- identifyCategoricalContinuousVars(X_train)
    o <- quadratic.discriminant.analysis(Y_train = Y_train, X_train = X_train, Y_test = Y_test, X_test = X_test)
    return(o)
  } else if (classifier == "dt") {
    ## Need to send more params
    ## params should default to checking via cross validation
    o <- decision.tree(Y_train = Y_train, X_train = X_train, Y_test = Y_test, X_test = X_test)
    return(o)
  } else if (classifier == "rf") {

    ## Need to send more params
    ## params should default to checking via cross validation
    if(missing(max.pred)){
      o <- random.forest(Y_train = Y_train, X_train = X_train, Y_test = Y_test, X_test = X_test)
    }else {
      o <- random.forest(Y_train = Y_train, X_train = X_train, Y_test = Y_test, X_test = X_test, max.pred = max.pred)
    }
    return(o)

  } else {
    #### DO ALL
    res.knn <- k.nearest.neighbour(Y_train = Y_train, X_train = X_train, Y_test = Y_test, X_test = X_test)
    res.nb <- naive.bayes(Y_train = Y_train, X_train = X_train, Y_test = Y_test, X_test = X_test)
    res.lr <- logistic.regression(Y_train = Y_train, X_train = X_train, Y_test = Y_test, X_test = X_test)
    res.lda <- linear.discriminant.analysis(Y_train = Y_train, X_train = X_train, Y_test = Y_test, X_test = X_test)
    res.qda <- quadratic.discriminant.analysis(Y_train = Y_train, X_train = X_train, Y_test = Y_test, X_test = X_test)
    res.dt <- decision.tree(Y_train = Y_train, X_train = X_train, Y_test = Y_test, X_test = X_test)
    res.rf <- random.forest(Y_train = Y_train, X_train = X_train, Y_test = Y_test, X_test = X_test)

    # Print ?
    aggregate.results(res.knn, res.nb, res.lr, res.lda, res.qda, res.dt, res.rf)
    plot_roc_curves(res.knn, res.nb, res.lr, res.lda, res.qda, res.dt, res.rf)

  }
}

############### PLOT/SUMMARY METHODS FOR results from classifiers ##########

printClassifierMetrics <- function(o){
  metrics <- classifier.metrics(o@prediction, print.flag = FALSE)
  cat('MSPE       : ', metrics[1], '\n',
      'Accuracy   : ', metrics[2], '\n',
      'Sensitivity: ', metrics[3], '\n',
      'Specificity: ', metrics[4], '\n',
      'Precision  : ', metrics[5], '\n', sep = "")

}

setGeneric("summarize", function(o){
  standardGeneric("summarize")
})
setMethod("summarize", c(o = 'knn.classifier'), function(o){
  cat('\n\n***************** KNN Classification **************************',
      '*****\n\n', sep = "")
  cat('K-Value                          : ', o@finalModel$k, '\n',
      'Number of Dimensions (predictors): ', length(o@finalModel$xNames), '\n',
      'Training set size                : ', length(o@finalModel$learn$y), '\n',
      'Test set size                    : ', length(slot(o@prediction,"labels")[[1]]), sep = "")

  cat('\n\n -------- Accuracy Measures -------- ',
      '\n\n', sep = "")

  printClassifierMetrics(o)

})

setMethod("summarize", c(o='nb.classifier'), function(o){

  cat('\n\n***************** Naive bayes Classification **************************',
      '*****\n\n', sep = "")
  cat('Number of predictors: ', length(o@finalModel$tables), '\n',sep="")

  cat('\n\n -------- Accuracy Measures -------- ',
      '\n\n', sep = "")

  printClassifierMetrics(o)
  cat('\n\n -------- Model Assumptions -------- ',
      '\n\n', sep = "")
  cat('The conditional probability distribution of each predictor for a given output is independent of other predictors', sep="")
})

setMethod("summarize",c(o = 'lda.classifier'), function(o){

  cat('\n\n***************** Linear Discriminant Analysis Classification **************************',
      '*****\n\n', sep = "")

  cat('Number of Dimensions (predictors): ', length(o@finalModel$scaling), '\n',
      'Training set size                : ', o@finalModel$N, '\n',
      'Test set size                    : ', length(slot(o@prediction,"labels")[[1]]), sep = "")
  cat('\n\n -------- Accuracy Measures -------- ',
      '\n\n', sep = "")

  printClassifierMetrics(o)
  cat('\n\n -------- Model Assumptions -------- ',
      '\n\n', sep = "")
  cat(' - All predictors are continuous', '\n',
    ' - All predictors have Normally distributed and independent conditional probabilities', '\n',
    ' - Same covariance matrix for ##########', sep="")

})
setMethod("summarize", c(o = 'qda.classifier'), function(o){

  cat('\n\n***************** Quadratic Discriminant Analysis Classification **************************',
      '*****\n\n', sep = "")

  cat('Number of Dimensions (predictors): ', length(o@finalModel$scaling), '\n',
      'Training set size                : ', o@finalModel$N, '\n',
      'Test set size                    : ', length(slot(o@prediction,"labels")[[1]]), sep = "")
  cat('\n\n -------- Accuracy Measures -------- ',
      '\n\n', sep = "")

  printClassifierMetrics(o)
  cat('\n\n -------- Model Assumptions -------- ',
      '\n\n', sep = "")
  cat(' - All predictors are continuous', '\n',
    ' - All predictors have Normally distributed and independent conditional probabilities', '\n',
     sep="")

})

setMethod("summarize", c(o = "rf.classifier"), function(o){

  cat('\n\n***************** Random Forest Classification **************************',
      '*****\n\n', sep = "")

  cat('Number of Dimensions (predictors): ', length(o@finalModel$xNames), '\n',
      'Number of Trees                  : ', o@finalModel$ntree, '\n',
      'Number of params in each tree    : ', o@finalModel$mtry, '\n',
      'Training set size                : ', length(o@finalModel$y), '\n',
      'Test set size                    : ', length(slot(o@prediction,"labels")[[1]]), sep = "")
  cat('\n\n -------- Accuracy Measures -------- ',
      '\n\n', sep = "")

  printClassifierMetrics(o)
})

setMethod("summarize", c(o = "dt.classifier"), function(o){

  cat('\n\n***************** Decision Tree Classification **************************',
      '*****\n\n', sep = "")

  cat( 'Training set size                : ', slot(slot(o@finalModel, "responses"), "nobs")[[1]], '\n',
      'Test set size                    : ', length(slot(o@prediction,"labels")[[1]]), sep = "")
  cat('\n\n -------- Accuracy Measures -------- ',
      '\n\n', sep = "")

  printClassifierMetrics(o)
  cat('\n\n -------- Tree Structure -------- ',
      '\n\n', sep = "")
  print(slot(o@finalModel, "tree"))
})

setMethod("summarize", c(o = "lr.classifier"), function(o){
  cat('\n\n***************** Logistic Regression Classification **************************',
      '*****\n\n', sep = "")

  cat('Number of Dimensions (predictors): ', length(attr(o@finalModel$terms,"term.labels")), '\n',
      'AIC                              : ', o@finalModel$aic, '\n',
      'Deviance                         : ', o@finalModel$deviance, '\n',
      'Null Deviance                    : ', o@finalModel$null.deviance, '\n',
      'Training set size                : ', length(o@finalModel$deviance.resid), '\n',
      'Test set size                    : ', length(slot(o@prediction,"labels")[[1]]), sep = "")

  cat('\n\n -------- Coefficients learned -------- ',
      '\n\n', sep = "")
  print(o@finalModel$coefficients)

  cat('\n\n -------- Accuracy Measures -------- ',
      '\n\n', sep = "")
  printClassifierMetrics(o)

})



#' Plot the results from the KNN classifier
#'
#'
#' This should plot the results from the knn classifier. It can
#' provide the user with an input to select the type of graph they want to see.
#'
#' @param The object returned by the classification function with class knn, the first element of this object is of type prediction
#'
#'
plot.knn <- function(o){
  per <- performance(o[[1]], measure = "fpr", x.measure = "fnr")
  print(class(o))
  ### This plot function is not working.
  plot(per)
}




