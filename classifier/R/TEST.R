
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
res.qda <- classification(data, outcome.col = "Class.Good", classifier = "qda")
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


# ALL
res.all <- classification(data, outcome.col = "Class.Good", classifier = "all")





