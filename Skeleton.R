library(caret, quietly = TRUE)
library(ROCR, quietly = TRUE)
library(class, quietly = TRUE)
library(MASS, quietly = TRUE)
library(e1071, quietly = TRUE)
library(ade4, quietly = TRUE)
library(randomForest, quietly = TRUE)
library(party, quietly = TRUE)

source('All_Generics.R')

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
# List of significant vars according to Random Forest or Decision Tree

#' Reposition response variable to first column of a dataframe
#'
#' @param X The dataframe with columns corresponding to predictors and rows corresponding to observations
#' @return The name of the variable
reposition.Y <- function(df, var.name){
  index = which(colnames(df) == var.name)
  df = cbind(df[,index], df[,-index])
  colnames(df)[1] = var.name
  return(df)
}


#' Identify predictors which are categorical or otherwise
#'
#' @param X The dataframe with columns corresponding to predictors and rows corresponding to observations
#' @return A named list of flags for categorical and continuous with names corresponding to variable names or column numbers
identifyNonNumericVars <- function(X){
  s = lapply(X, class)
  # Returns categorical as TRUE
  output = ((s != "numeric") & (s != "integer"))
  return(output)
}

#' Identify variables consisting of one value and remove them
#'
#' @param X The dataframe with columns corresponding to predictors and rows corresponding to observations
#' @return X the dataframe with the constant variables removed
remove.constant.variables <- function(X){
  # Identify constants
  constants = which(unlist(lapply(X, function(x) length(unique(x))))==1)
  if (length(constants) > 0){
    X = X[,-constants]
  }
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
  Y = as.factor(as.character(data.matrix(Y)))
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
  return(unlist(var_mean))

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
  Xp <- prcomp(X, scale = TRUE)
  X.var <- Xp$sdev^2
  pve <- X.var / sum(X.var)
  names(pve) = paste('PC', 1:ncol(X), sep="")
  return(pve)
}

#' Point out highly collinear variables
#'
#'
#' @param X The dataframe with columns corresponding to predictors and rows corresponding to observations
#' @return
predictorCollinearity <- function(X, threshold = 0.9){
  p = ncol(X)
  df_cor <- as.data.frame(as.table(cor(X)))
  # Remove correlation with self
  df_cor = subset(df_cor,df_cor$Var1 != df_cor$Var2)
  # Remove duplicates
  df_cor = df_cor[!duplicated(df_cor$Freq),]
  # Sort descending by absolute correlation
  df_cor = df_cor[ with(df_cor, order( -abs(df_cor[,3]) )),]
  rownames(df_cor) <- 1:nrow(df_cor)
  # Subset those higher than threshold
  df_cor = subset(df_cor, abs(df_cor$Freq) > threshold)
  colnames(df_cor)[3] = 'Correlation'

  if (nrow(df_cor) == 0){
    cat('There are no collinear variables in the dataset')
  }else{
    cat('The following variables show signs of collinearity:\n')
    print.data.frame(df_cor, right = FALSE, digits = 3)
  }
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
      X = cbind(X, X_temp[-1])
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
  output <- NULL
  res <- tryCatch(
    {
      ctrl <- trainControl(method = "cv", number = nfolds, savePredictions = TRUE)
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
      output = new("knn.classifier", prediction = pr, finalModel = cv.fit$finalModel)
    }, error = function(e){
      msg <- paste0("K Nearest Neighbours Failed. ", e$message)
      print(msg)
    })

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

  output <- NULL
  res <- tryCatch(
    {
      Y_train2 <- as.factor(as.character(data.matrix(Y_train)))
      model = naiveBayes(Y_train2~. , data = X_train)
      pred <- predict(model, newdata = as.data.frame(X_test))
      pr <- prediction(as.numeric(as.character(pred)), Y_test)
      output = new("nb.classifier", prediction = pr, finalModel = model)
    }, error = function(e){
      msg <- paste0("Naive Bayes Failed. ", e$message)
      print(msg)
    })
  return(output)
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
logistic.regression <- function(Y_train, X_train, Y_test, X_test, threshold.prob = NULL){

  output <- NULL
  res <- tryCatch(
    {
      # Copy the training set
      X_train2 = data.matrix(X_train)
      Y_train2 = as.factor(as.character(data.matrix(Y_train)))
      # Fit initial model
      glm.fit <- glm(Y_train2 ~. , data=as.data.frame(X_train2), family = binomial)
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
        glm.fit <- glm(Y_train2 ~. , data=as.data.frame(X_train2), family = binomial)
        s = summary(glm.fit)
      }
      # Now obtain probabilities
      glm.probs <- predict(glm.fit, type = "response")
      d1 <- length(glm.probs)

      # Optimize threshold probability to classify 0/1 in the training set
      if (is.null(threshold.prob)){
        lr_accuracy = c()
        cutoff = seq(0, 1, 0.01)
        for (i in cutoff){
          glm.pred.train <- rep(0, d1)
          glm.pred.train[glm.probs > i] = 1
          accur = mean(glm.pred.train == Y_train)
          lr_accuracy = c(lr_accuracy, accur)
        }
        ind = which(lr_accuracy == max(lr_accuracy))[1]
        threshold.prob = cutoff[ind]
      }

      # Predict in training set using the optimal threshold
      glm.pred.train <- rep(0, d1)
      glm.pred.train[glm.probs > threshold.prob] = 1
      accur = mean(glm.pred.train == Y_train)
      # Predict in test set
      X_test2 = X_test[,colnames(X_train2)]
      glm.probs <- predict(glm.fit, newdata = as.data.frame(X_test2), type = "response")
      d2 = length(glm.probs)
      glm.pred.test <- rep(0, d2)
      glm.pred.test[glm.probs > threshold.prob ] = 1
      pr <- prediction(as.numeric(as.character(glm.pred.test)), Y_test)
      # Return model and prediction objects
      output = new("lr.classifier", prediction = pr, finalModel = s)
    }, error = function(e){
      msg <- paste0("Logistic Regression Failed.", e$message)
      print(msg)
    })
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
  output <- NULL
  res <- tryCatch(
    {
      Y_train2 <- as.factor(as.character(data.matrix(Y_train)))
      model = lda(Y_train2 ~ ., data = X_train)
      pred <- predict(model, newdata = as.data.frame(data.matrix(X_test)))
      pr <- prediction(as.numeric(as.character(pred$class)), Y_test)
      output = new("lda.classifier", prediction = pr, finalModel = model)
    }, error = function(e){
      msg <- paste0("Linear Discriminant Analysis Failed. ", e$message)
      print(msg)
    })
  return(output)
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
  output <- NULL
  res <- tryCatch(
    {
      Y_train2 <- as.factor(as.character(data.matrix(Y_train)))
      model = qda(Y_train2 ~ ., data = X_train)
      pred <- predict(model, newdata = as.data.frame(data.matrix(X_test)))
      pr <- prediction(as.numeric(as.character(pred$class)), Y_test)
      output <- new("qda.classifier", prediction = pr, finalModel = model)
    }, error = function(e){
      msg <- paste0("Quadratic Discriminant Analysis Failed. ", e$message)
      print(msg)
    })
  return(output)
}

#'
#'
#'
#'
#' @param X_train The dataframe with columns corresponding to predictors and rows corresponding to observations
#' @param X_test The dataframe with columns corresponding to predictors and rows corresponding to observations
#' @param Y_train
#' @param Y_test
#' @param max.level
#' @param nfolds number of folds for cross validation
#' @return fitted model and prediction object
decision.tree <- function(Y_train, X_train, Y_test, X_test, max.level = 5, nfolds = 4){
  output <- NULL
  res <- tryCatch(
    {
      # Perform cross validation
      ctrl <- trainControl(method = "cv", number = nfolds, savePredictions = TRUE)
      Y_train2  = as.factor(as.character(data.matrix(Y_train)))
      # Fit with max level specified
      cv.fit <- train(Y_train2 ~ ., data = data.matrix(X_train), method="ctree",
                      trControl = ctrl, tuneLength = 10, controls = ctree_control(maxdepth = max.level))
      # Predict with new data
      tree.pred = predict(cv.fit, newdata = as.data.frame(data.matrix(X_test)), type = 'raw')
      pr <- prediction(as.numeric(as.character(tree.pred)), Y_test)
      # Return model and prediction objects
      output = new("dt.classifier", prediction = pr, finalModel = cv.fit$finalModel)
    }, error = function(e){
      msg <- paste0("Decision Tree Failed.", e$message)
      print(msg)
    })

  return(output)
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
#' @param max.pred the number of parameters that each tree will have
#' @param max.level
#' @return fitted model and prediction object
random.forest <- function(Y_train, X_train, Y_test, X_test, B, max.pred = 4,max.level = 6, nfolds = 4){
  # Perform cross validation
  output <- NULL
  res <- tryCatch(
    {
      ctrl <- trainControl(method = "cv", number = nfolds, savePredictions = TRUE)
      Y_train2  = as.factor(as.character(data.matrix(Y_train)))
      cv.fit <- train(Y_train2 ~ ., data = data.matrix(X_train), method="rf",
                      trControl = ctrl, tuneLength = 2, controls = ctree_control(mtry = max.pred,
                                                                                 maxdepth = max.level))
      rf.pred = predict(cv.fit, newdata = as.data.frame(data.matrix(X_test)), type = 'raw')
      pr <- prediction(as.numeric(as.character(rf.pred)), Y_test)
      # Return model and prediction objects
      output = new("rf.classifier", prediction = pr, finalModel = cv.fit$finalModel)
    }, error = function(e){
      msg <- paste0("Random Forest Failed. ", e$message)
      print(msg)
    })
  return(output)

}


########## OUTPUT RELATED ###############
# Summary of prediction objects
# Provide list of model assumptions for each model specified
# Plot Accuracy/ROC/AUC/MSE/MSPE Curves

classifier.metrics <- function(pred.obj, print.flag = FALSE){
  "Return classifier statistics in the test set
  Input: Prediction object (ROCR)
  Output: A list with MSPE, accuracy, sensitivity, specificity and precision"
  mspe = mspe = mean((slot(pred.obj, 'predictions')[[1]] -
                        as.numeric(as.character(slot(pred.obj, 'labels')[[1]])))^2)
  accuracy = slot(performance(pred.obj, "acc"), "y.values")[[1]][2]
  sensitivity = slot(performance(pred.obj, "sens"), "y.values")[[1]][2]
  specificity = slot(performance(pred.obj, "spec"), "y.values")[[1]][2]
  precision = slot(performance(pred.obj, "prec"), "y.values")[[1]][2]

  if (print.flag){
    cat('\n- Classifier metrics:\n   MSPE: ', mspe, '\n   Accuracy: ',
        accuracy, '\n   Sensitivity: ', sensitivity, '\n   Specificity: ',
        specificity, '\n   Precision: ', precision, '\n')
  }
  return(c(mspe, accuracy, sensitivity, specificity, precision))
}

barplot.classifier.metric <- function(df_col, name, labels){
  ind = which(df_col == max(df_col))
  barcol = rep('aliceblue', length(labels))
  barcol[ind] = 'darkblue'
  barplot(100 * df_col, main = name, col = barcol, ylab = paste(name, '(%)'),
          names.arg = labels, ylim = c(0,100))
  par(xpd=TRUE)
  text(ind - 0.5 + 0.2 * ind, max(100 * df_col) + 6 , paste(round(max(100 * df_col),1),'%',sep=''))
  # box()
}

#'
#'
#'
#'@param output is an S4 object of type "all.classifier" which contains all the classifiers which have been trained as 'slots'
#'
#'
aggregate.results <- function(output){
  "Stores results in a dataframe
  Input: Prediction objects (ROCR)"

  # Aggregate results in a dataframe
  df_res = data.frame('Classifier' = character(7),
                      'MSPE_test' = double(7),
                      'Accuracy' = double(7),
                      'Sensitivity' = double(7),
                      'Specificity' = double(7),
                      'Precision' = double(7),
                      'Ranking' = character(7), stringsAsFactors=FALSE )

  name.list = list(knn.classifier = 'KNN',
                   nb.classifier  = 'NB',
                   lr.classifier  = 'LR',
                   lda.classifier = 'LDA',
                   qda.classifier = 'QDA',
                   dt.classifier  = 'DT',
                   rf.classifier  = 'RF')

  # Choose those classifiers which have non null outputs
  j = 1
  for(name in slotNames(output)){
    if(!is.null(slot(output, name))){
      df_res[j,1]    = name.list[[name]]
      df_res[j, 2:6] = classifier.metrics(slot(output, name)@prediction)
      j = j + 1
    }
  }
  df_res = df_res[-j,]


  ranking = as.data.frame(sapply(df_res[,3:6], function(x) rank(x)))
  ranking$avg = 0
  for (i in nrow(df_res)){
    ranking$avg[i] = mean(as.double(ranking[i,1:4]))
  }
  ind = which(ranking$avg == max(ranking$avg))
  df_res[ind,7] = 'BEST'
  options(digits = 3)
  cat('\n')
  print.data.frame(df_res, digits = 3, right = FALSE)
  cat('\n')

  # Plot
  par(mfrow = c(2,2))
  barplot.classifier.metric(df_res[,3], 'Accuracy', df_res[,1])
  barplot.classifier.metric(df_res[,6], 'Precision', df_res[,1])
  barplot.classifier.metric(df_res[,4], 'Sensitivity',df_res[,1])
  barplot.classifier.metric(df_res[,5], 'Specificity', df_res[,1])
}

#'
#'
#'
#'@param output is an S4 object of type "all.classifier" which contains all the classifiers which have been trained as 'slots'
#'
#'
plot_roc_curves <- function(output){
  # Obtain true and false positives

  name.list = list(knn.classifier = 'KNN',
                   nb.classifier  = 'NB',
                   lr.classifier  = 'LR',
                   lda.classifier = 'LDA',
                   qda.classifier = 'QDA',
                   dt.classifier  = 'DT',
                   rf.classifier  = 'RF')
  par(mfrow = c(1,1))
  colors = c('blue', 'red', 'darkgreen', 'deeppink3', 'goldenrod2', 'darkolivegreen3')
  i = 1
  labels <- c()
  rocs <- list()
  for(name in slotNames(output)){
    if(!is.null(slot(output, name))){
      roc = performance(slot(output, name)@prediction, measure = 'tpr', x.measure = 'fpr')
      if(i == 1){
        plot(roc, col = colors[i], lwd = 3, main = 'ROC curve per classifier',
             xlim = c(0,1), ylim = c(0,1))
      }else {
        plot(roc, col = colors[i], lwd = 2, add=TRUE)
      }
      labels[i] <- name.list[[name]]
      i = i+1
    }
  }
  par(xpd=FALSE)
  abline(a=0, b=1, lwd=2)

  legend(0.52, 0.45, labels, lty=c(1,1), lwd=c(2.5, 2.5, 2.5, 2.5), col=
           colors[1:i])
}

