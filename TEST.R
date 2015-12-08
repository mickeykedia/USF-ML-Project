
# Clean workspace
rm(list=ls())

source("Classification.R")


########### PREPROCESSING ##############

data <- read.table('skin_nonskin.txt', header = FALSE)
colnames(data) <- c('B','G', 'R', 'Skin')
data <- reposition.Y(data, 'Skin')
str(data)


########## PREDICTOR EVALUATION ##########
classification.predictor.evaluation(data)

# Subset to speed up test
index <- sample(nrow(data), 6000)
data <- data[index, ]


########## MODEL SELECTION ##############
res.all <- classification(data, outcome.col = "Skin", classifier = "all")


########## INDIVIDUAL CLASSIFIERS ###########

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
summarize(res.qda)
plot.summary(res.qda)

res.rf <- classification(data, outcome.col = "Skin", classifier = "rf", max.pred = 2)
summarize(res.rf)
plot.summary(res.rf)

res.dt <- classification(data, outcome.col = "Skin", classifier = "dt")
summarize(res.dt)
plot.summary(res.dt)

res.lr <- classification(data, outcome.col = "Skin", classifier = "lr")
summarize(res.lr)
plot.summary(res.lr)



############# PREDICTIONS ###################


