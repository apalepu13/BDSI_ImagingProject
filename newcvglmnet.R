library(MASS)
library(caret)
library(glmnet)
library(jpeg)
library(png)
library(radiomics)
library(imager)

training.index = (1:5800)
test.index = (5801:6300)
impath <- "/home/apalepu/bdsi_imaging/Anil/Training"
filePath <- "D:/Documents/bdsi_imaging/Anil/R_Files"
setwd(filePath)
features <- read.table("allFeatures2.txt", header = TRUE)
features[ , 136:149][is.na(features[ , 136:149] ) ] = .5
trainFeatures = features[training.index,]
testFeatures = features[test.index,]
labels<- read.csv("allTrainingLabels.csv", header = TRUE)[,3]
labels <- ifelse(labels=="Melanoma",1,0)
trainLabels = labels[training.index]
testLabels = labels[test.index]

cvfit = cv.glmnet(as.matrix(trainFeatures), trainLabels,family="binomial",nfolds=10)
cvfit$lambda.min
a <- coef(cvfit, s = "lambda.min")
a
indices = slot(a, "i")

trainFeaturesLabels <- cbind(trainFeatures[,indices], trainLabels)
z <- lda(trainLabels ~ ., trainFeaturesLabels, prior = c(.5, .5))
testFeatures2 <- testFeatures[,indices]
pr<- predict(z, testFeatures2)
predictions <- pr$class

statResults<-confusionMatrix(predictions, factor(testLabels), positive = '1')
statResults

