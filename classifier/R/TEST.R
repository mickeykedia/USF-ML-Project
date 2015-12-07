
##### TESTING FUNCTIONALITY #####
#####        SKIN DATA      #####

# Clean workspace
rm(list=ls())

# setwd("~/Google Drive/USF/MSAN_621_Intro_to_Machine_Learning/Final project/mlgit/USF-ML-Project/classifier/R")
source("Classification.R")

data =read.table('skin_nonskin.txt', header = FALSE)
colnames(data) = c('B','G', 'R', 'Skin')
data = reposition.Y(data, 'Skin')
str(data)

classification.param.evaluation(data)

# Subset to speed up test
index = sample(nrow(data), 6000)
data = data[index, ]


# Testing individual classifiers
res.knn <- classification(data, outcome.col = "Skin", classifier = "knn")
summarize(res.knn)
plot.summary(res.knn)
res.nb <- classification(data, outcome.col = "Skin", classifier = "nb")
summarize(res.nb)
plot.summary(res.nb)
res.lda <- classification(data, outcome.col = "Skin", classifier = "lda")
summarize(res.lda)
plot.summary(res.lda)
res.qda <- classification(data, outcome.col = "Skin", classifier = "qda")
summary(res.qda)
res.rf <- classification(data, outcome.col = "Skin", classifier = "rf", max.pred = 2)
summarize(res.rf)
plot.summary(res.rf)
res.dt <- classification(data, outcome.col = "Skin", classifier = "dt")
summarize(res.dt)
plot.summary(res.dt)
res.lr <- classification(data, outcome.col = "Skin", classifier = "lr")
summarize(res.lr)
plot.summary(res.lr)

# Ok!

# ALL
res.all <- classification(data, outcome.col = "Skin", classifier = "all")





##### GERMAN DATA #####

rm(list=ls())
source("Classification.R")

data("GermanCredit")
data = GermanCredit
data = reposition.Y(data, 'Class')
data = remove.constant.variables(data)
# data <- convertCategoricalToDummy(data)
# data = remove.constant.variables(data)
# data = reposition.Y(data, 'Class.Good')

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





