# MSAN621 - Machine Learning
# Final Project: Classification

library(caret, quietly = TRUE)
library(ROCR, quietly = TRUE)
library(class, quietly = TRUE)
library(ade4, quietly = TRUE)

# (1) PREPARE DATA FUNCTION ###################################################
prepare.data <- function(X, Y = NULL, remove.na = FALSE, categtodummy = FALSE, 
                       remove.collinear = FALSE, scale = FALSE,
                       corr_threshold = 0.9, print.flag = TRUE){
  "Performs the following operations:
  - Remove observations with NA values
  - Convert categorical variables to dummy variable.names
  - Remove collinear variables
  - Scale variable
  Input: Dataframes X, Y, and TRUE in one method
  Output: Dataframes X, Y"
  
  if (print.flag) cat('\n##### DATA PREPARTION #####')
  
  if (!is.null(Y)){
    df = cbind(Y,X) 
  }else{
    df = X
  }
  
  p = ncol(df)
  if (print.flag) cat('\n- Cleaning data: ')
  
  ### (A) Remove observations with NA values
  if (remove.na){
    if (print.flag) cat('\n- Number of NA values in the dataset:', sum(is.na(df)))
    df = na.omit(df)
    if (print.flag) cat('\n- Observations with NA values removed. ',
      '\n- Number of observations in the new dataset:', nrow(df), '\n')
  }
  
  ### (B) Remove collinear variables
  if (remove.collinear){
    if (print.flag) cat('\nRemoving collinear variables: ')
    # Calculate correlation matrix
    df_cor = data.frame(abs(cor(na.omit(df))))
    to_delete = c()
    for (i in 2:(p-1)){
      max_c = sort(df_cor[,i],partial=p-2)[p-2]
      if (max_c > 0.9){
        ind = which(df_cor[,i] == max_c)
        if ((df_cor[1,i] > df_cor[1,ind])){
          to_delete = c(to_delete,ind)
        } else{
          to_delete = c(to_delete,i)
        }
      }
    }
    to_delete = unique(to_delete)
    if (print.flag) cat('\n- Collinear variables removed:', 
                        paste(colnames(df)[to_delete], ' '))
    if (!is.null(to_delete)){
      df = df[,-to_delete]
    }
  }
  
  ### (C) Convert categorical to dummy
  to_del = c()
  if (categtodummy){
    if (print.flag) cat('\nConverting categorical variables to dummy ')
    for (i in 1:p){
      if (is.factor(df[,i])){
        
        df_temp = acm.disjonctif(df[,i, drop=FALSE])
        df = cbind(df, df_temp)
        to_del[length(to_del)+1] = i
      }
    }
    if (print.flag) cat('\n- Variables converted:', colnames(df)[to_del])
    df = df[,-to_del]
  }
  
  return(df)
}


# (4) K-NEAREST NEIGHBOR ######################################################

k.nearest.neighbor <- function(Y_train, X_train, Y_test, X_test, 
                               print.flag = TRUE){
  "Centers non-binary predictors,  runs cross-validation to find optimal k 
  and returns final model and prediction objects
  Input: Y_train, X_train, Y_test, X_test, print.flag
  Output: fitted model, prediction object"
  
  if (print.flag) cat('\n##### K NEAREST NEIGHBOR (KNN) #####',
    '\nRunning KNN classifier:')
  # Center the data
  n1 = nrow(X_train)
  X_temp = rbind(X_train, X_test)
  for (i in 1:ncol(X_temp)){
    if (sum(!(unique(X_temp[,i]) %in% c(1,0)))==0){
      X_temp[,i] = scale(X_temp[,i])
    }
  }
  X_train = X_temp[1:n1,]
  X_test = X_temp[(n1+1):nrow(X_temp),]
  
  if (print.flag) cat('\n- Data scaled')
  
  # Run 4-fold cross validation to find optimal k
  ctrl <- trainControl(method = "repeatedcv", number = 4, savePredictions = TRUE)
  Y_train2  = as.factor(as.character(data.matrix(Y_train)))
  cv.fit <- train(Y_train2 ~ ., data = data.matrix(X_train), method="knn",
                  trControl = ctrl, tuneLength = 10)
  optimal_k = cv.fit$bestTune[[1]]

  if (print.flag) cat('\n- Using 4-fold cross validation to find the best ',
  'parameter k \n- Optimal k:', optimal_k)
  
  # Predict
  knn.pred = predict(cv.fit$finalModel, newdata = as.data.frame(X_test), type = "class")
  pr <- prediction(as.numeric(as.character(knn.pred)), Y_test)
  
  output = c(pr, cv.fit$finalModel)
  if (print.flag){
      res = classifier.metrics(output, print.flag = TRUE)
  } 
  # Return model and prediction objects
  return(output)
}

# (6) LOGISTIC REGRESSION #####################################################
# Assumptions: binary dependent variable, independent residuals, no
# multicolinearity, sample size > 30
logistic.regression <- function(Y_train, X_train, Y_test, X_test, 
        print.flag = TRUE){
  "Runs logistic regression, removes statistically insignificant variables in 
  the regression, runs cross-validation to find optimal coefficients, 
  finds threshold probability that optimizes classification and returns
  final model and prediction objects
  Input: Y_train, X_train, Y_test, X_test, print.flag
  Output: fitted model, prediction object"
  
  if (print.flag) cat('\n##### LOGISTIC REGRESSION #####',
    '\nRunning logistic regression:')
  # Copy the training set
  X_train2 = data.matrix(X_train)
  Y_train2 = data.matrix(Y_train)
  
  # Fit initial model
  glm.fit <- glm(Y_train2 ~ X_train2, family = binomial)
  s = summary(glm.fit)
  if (print.flag) cat('\n- Fitting initial model with all variables',
  '\n- Removing statistically insignificant predictors (backward stepwise)')
  
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
  
  if (print.flag) cat('\n- Number of variables retained: ', ncol(X_train2), 
                    '\n- Summary of the regression:')
  if (print.flag) print(s)
  
  # Now run cross validation to find the optimal coefficients with those variables
  ctrl <- trainControl(method = "repeatedcv", number = 4, savePredictions = TRUE)
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
  
  if (print.flag) cat('\n- Optimal threshold probability for binary',
  'classification in the training set: ', cutoff[ind])
  
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
  
  output = c(pr, cv.fit$finalModel)
  if (print.flag){
    res = classifier.metrics(output, print.flag = TRUE)
  }
  # Return model and prediction objects
  return(output)
}


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



# (10) AGGREGATE RESULTS ######################################################
aggregate.results <- function(res.knn, res.nb, res.log, res.lda, res.qda, 
                              res.tree, res.rf){
  "Stores results in a dataframe
  Input: Prediction objects (ROCR)"
  
  # Aggregate results in a dataframe
  df_res = data.frame('Classifier' = character(7), 
                      'MSPE_test' = double(7), 
                      'Accuracy' = double(7), 
                      'Sensitivity' = double(7), 
                      'Specificity' = double(7))
  df_res[,1] = c('K-Nearest Neighbor', 'Naive Bayes', 'Logistic Regression', 
                 'Linear Discriminant Analysis', 'Quadratic Discriminant Analysis', 
                 'Decision Tree', 'Random Forests')
  df_res[1,2:5] = classifier.metrics(res.knn)
  df_res[2,2:5] = classifier.metrics(res.nb)
  df_res[3,2:5] = classifier.metrics(res.log)
  df_res[4,2:5] = classifier.metrics(res.lda)
  df_res[5,2:5] = classifier.metrics(res.qda)
  df_res[6,2:5] = classifier.metrics(res.tree)
  df_res[7,2:5] = classifier.metrics(res.rf)
  print.data.frame(df_res, digits = 3, right = FALSE)
}

# (10) PLOT ROC CURVES ######################################################

plot_roc_curves <- function(res.knn, res.nb, res.log, res.lda, res.qda, 
                            res.tree, res.rf){
  # Obtain true and false positives
  roc.1 = performance(res.knn[[1]], measure = 'tpr', x.measure = 'fpr')
  roc.2 = performance(res.nb[[1]], measure = 'tpr', x.measure = 'fpr')
  roc.3 = performance(res.log[[1]], measure = 'tpr', x.measure = 'fpr')
  roc.4 = performance(res.lda[[1]], measure = 'tpr', x.measure = 'fpr')
  roc.5 = performance(res.qda[[1]], measure = 'tpr', x.measure = 'fpr')
  roc.6 = performance(res.tree[[1]], measure = 'tpr', x.measure = 'fpr')
  roc.7 = performance(res.rf[[1]], measure = 'tpr', x.measure = 'fpr')
  
  # Plot all together
  par(mfrow = c(1,1))
  plot(roc.1, col = 'beige', lwd = 3, main = 'ROC curve per classifier')
  plot(roc.2, add=TRUE, col = 'blue', lwd = 2)
  plot(roc.3, add=TRUE, col = 'red', lwd = 2)
  plot(roc.4, add=TRUE, col = 'darkgreen', lwd = 2)
  plot(roc.5, add=TRUE, col = 'magenta', lwd = 2)
  plot(roc.6, add=TRUE, col = 'yellow', lwd = 2)
  plot(roc.7, add=TRUE, col = 'aliceblue', lwd = 2)
  abline(a=0, b=1, lwd=2)
  legend(0.52, 0.45, c('KNN', 'Naive Bayes', 'Logistic', 'LDA', 'QDA', 'Decision Tree', 'Random Forests'), lty=c(1,1), lwd=c(2.5, 2.5, 2.5, 2.5), col=
           c('blue', 'red', 'darkgreen', 'magenta', 'yellow', 'aliceblue') )
}


