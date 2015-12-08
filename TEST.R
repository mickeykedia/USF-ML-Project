
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


############### SUBSETTING AND TRAIN/TEST SPLIT ##########
index <- sample(nrow(data), 7000)
test.index <- index[1:1000]
train.index <- index[1001:length(index)]
test.data <- data[test.index, ]
data <- data[train.index, ]


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


results <- predict(res.knn@finalModel, test.data[-1], type = "class")
pred <- prediction(as.numeric(as.character(results)), test.data[1])
classifier.metrics(pred, print.flag = TRUE)
