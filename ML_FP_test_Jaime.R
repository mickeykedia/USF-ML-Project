# MSAN621 - Machine Learning
# Final Project: Classification

# (This is unfinished)

# Clean workspace 
rm(list=ls())
setwd("~/Google Drive/USF/MSAN_621_Intro_to_Machine_Learning/Final project")

# Functions stored in ML_FP_functions_Jaime.R

source('ML_FP_functions_Jaime.R')

# Let's load a dataset to develop:
df = read.csv('titanic3.csv')
# Replace NAs with mean
df$age[is.na(df$age)] = mean(df$age, na.rm = TRUE)
df$fare[is.na(df$fare)] = mean(df$fare, na.rm = TRUE)

# head(prepare.data(df[,c(1,2,4)], categtodummy = TRUE))
# head(prepare.data(df, remove.na = TRUE))
# prepare.data(df[,c(2,5,6)], df[,1], remove.collinear = TRUE)
# df = df[,c(1,2,5,6)]

# Y = survived
# X = pcclass, sex(1/0), age, sibsp, parch, fare
# df = na.omit(df)
Y = df[,c('survived'), drop=FALSE]
X = df[, c('pclass', 'sex', 'age', 'sibsp', 'parch', 'fare')]
X$sex = (df$sex == 'male') + 0


# Check NAs
sum(is.na(X))
sum(is.na(Y))

# (3) SPLIT DATASET ###########################################################
# Randomly sample to separate training and test sets
proportion_test = 0.2
index_test = sample(nrow(X), round(nrow(X) * proportion_test,0))
X_train = X[-index_test,]
Y_train = Y[-index_test, , drop=FALSE]
X_test = X[index_test,]
Y_test = Y[index_test, , drop=FALSE]

# (4) K-NEAREST NEIGHBOR ######################################################

# Run KNN classifier
res.knn = k.nearest.neighbor(Y_train, X_train, Y_test, X_test)

# (6) LOGISTIC REGRESSION #####################################################

# Run Logistic regression classifier
res.log = logistic.regression(Y_train, X_train, Y_test, X_test)

# Pretending we have the rest
res.nb = res.knn
res.lda = res.knn
res.qda = res.knn
res.tree = res.knn
res.rf = res.knn

res.log = logistic.regression(Y_train, X_train, Y_test, X_test)

# Pretending we have the rest
res.nb = res.knn
res.lda = res.knn
res.qda = res.knn
res.tree = res.knn
res.rf = res.knn


# (10) AGGREGATE RESULTS ######################################################

# Aggregate and print results in a table
aggregate.results(res.knn, res.nb, res.log, res.lda, res.qda, res.tree, res.rf)

# (10) PLOT ROC CURVES ######################################################

# Plot ROC curves
plot_roc_curves(res.knn, res.nb, res.log, res.lda, res.qda, res.tree, res.rf)


