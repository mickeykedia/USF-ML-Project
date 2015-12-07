
##### TESTING FUNCTIONALITY #####

# Clean workspace
rm(list=ls())

setwd("~/Google Drive/USF/MSAN_621_Intro_to_Machine_Learning/Final project/mlgit/USF-ML-Project/classifier/R")
source("Classification.R")

data("GermanCredit")
data = GermanCredit
data <- convertCategoricalToDummy(data)
data = remove.constant.variables(data)
data = reposition.Y(data, 'Class.Good')

classification.param.evaluation(data)


# Testing individual classifiers
res.knn <- classification(data, outcome.col = "Class.Good", classifier = "knn")
summarize(res.knn)
plot.summary(res.knn)
res.nb <- classification(data, outcome.col = "Class.Good", classifier = "nb")
summarize(res.nb)
plot.summary(res.nb)
res.lda <- classification(data, outcome.col = "Class.Good", classifier = "lda")
summarize(res.lda)
plot.summary(res.lda)
#### DOESN'T WORK ######
#res.qda <- classification(data, outcome.col = "Class.Good", classifier = "qda")
# summary(res.qda)
res.rf <- classification(data, outcome.col = "Class.Good", classifier = "rf", max.pred = 10)
summarize(res.rf)
plot.summary(res.rf)
res.dt <- classification(data, outcome.col = "Class.Good", classifier = "dt")
summarize(res.dt)
plot.summary(res.dt)
res.lr <- classification(data, outcome.col = "Class.Good", classifier = "lr")
summarize(res.lr)
plot.summary(res.lr)

# Fake qda (data is rank defficient)
res.qda = res.lr

# Aggregate results
aggregate.results(res.knn, res.nb, res.lr, res.lda, res.qda, res.dt, res.rf)
plot_roc_curves(res.knn, res.nb, res.lr, res.lda, res.qda, res.dt, res.rf)

# Ok!

# ALL
res.all <- classification(data, outcome.col = "Class.Good", classifier = "all")






# PREVIOUS TESTS
################ TESTING INDIVIDUAL CLASSIFIERS #######################

data = data("GermanCredit")
data <- convertCategoricalToDummy(GermanCredit)
data = remove.constant.variables(data)




res.knn <- classification(data, outcome.col = "credit_risk.bad", classifier = "knn")
# classifier.metrics(res.knn, print.flag = TRUE)

res.nb <- classification(data, outcome.col = "credit_risk.bad", classifier = "nb")
# classifier.metrics(res.nb, print.flag = T)
res.lda <- classification(data, outcome.col = "credit_risk.bad", classifier = "lda")
res.qda <- classification(data, outcome.col = "credit_risk.bad", classifier = "qda")
res.rf <- classification(data, outcome.col = "credit_risk.bad", classifier = "rf")
# classifier.metrics(res.rf, print.flag = T)
res.dt <- classification(data, outcome.col = "credit_risk.bad", classifier = "dt")
# classifier.metrics(res.dt, print.flag = T)

res.lr <- classification(data, outcome.col = "credit_risk.bad", classifier = "lr")
classifier.metrics(res.lr, print.flag = T)

aggregate.results(res.knn, res.nb, res.lr, res.lda, res.qda, res.dt, res.rf)
plot_roc_curves(res.knn, res.nb, res.lr, res.lda, res.qda, res.dt, res.rf)



#################################################################################
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

data("GermanCredit")
data = GermanCredit
#data = cbind(data[,8], data[,-8])
data <- convertCategoricalToDummy(GermanCredit)
data = remove.constant.variables(data)

# classification.param.evaluation(data)
o <- classification(data = data, outcome.col = "credit_risk.bad", classifier = "knn")
o
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


