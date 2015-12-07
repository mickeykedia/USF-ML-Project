
### DEFINING S4 CLASSES ################
setClass("knn.classifier", slots = list(prediction = "prediction", finalModel = "ANY"))
setClass("nb.classifier", slots = list(prediction = "prediction", finalModel = "ANY"))
setClass("lr.classifier", slots = list(prediction = "prediction", finalModel = "ANY"))
setClass("lda.classifier", slots = list(prediction = "prediction", finalModel = "ANY"))
setClass("qda.classifier", slots = list(prediction = "prediction", finalModel = "ANY"))
setClass("rf.classifier", slots = list(prediction = "prediction", finalModel = "ANY"))
setClass("dt.classifier", slots = list(prediction = "prediction", finalModel = "ANY"))

########## DEFINE plot.summary ###############
setGeneric("plot.summary", function(o){

})

setMethod("plot.summary", c(o = 'knn.classifier'), function(o){
  roc.1 = performance(o@prediction, measure = 'tpr', x.measure = 'fpr')
  plot(roc.1)
})
setMethod("plot.summary", c(o = 'nb.classifier'), function(o){
  roc.1 = performance(o@prediction, measure = 'tpr', x.measure = 'fpr')
  plot(roc.1)
})
setMethod("plot.summary", c(o = 'lda.classifier'), function(o){
  roc.1 = performance(o@prediction, measure = 'tpr', x.measure = 'fpr')
  plot(roc.1)
})
setMethod("plot.summary", c(o = 'qda.classifier'), function(o){
  roc.1 = performance(o@prediction, measure = 'tpr', x.measure = 'fpr')
  plot(roc.1)
})
setMethod("plot.summary", c(o = 'lr.classifier'), function(o){
  roc.1 = performance(o@prediction, measure = 'tpr', x.measure = 'fpr')
  plot(roc.1)
})
setMethod("plot.summary", c(o = 'dt.classifier'), function(o){
  roc.1 = performance(o@prediction, measure = 'tpr', x.measure = 'fpr')
  plot(roc.1)
})
setMethod("plot.summary", c(o = 'rf.classifier'), function(o){
  roc.1 = performance(o@prediction, measure = 'tpr', x.measure = 'fpr')
  plot(roc.1)
})

########### DEFINE SUMMARIZE #############
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
