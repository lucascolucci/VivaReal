#In this file we will apply what we discovered in Desafio_Analysis.R to generate the predictions.

library(data.table)
library(sm)
library(DAAG)
library(mice)
library(caret)

setwd("Desktop/Desafio VivaReal/")

trainData <- read.csv("challenge_datasets/train.csv")
testData  <- read.csv("challenge_datasets/test.csv")

#Prepare datasets
trainData$feature_8 <- NULL
trainData$feature_13 <- NULL
trainData$feature_12 <- NULL
trainData[,c("feature_1","feature_2","feature_3","feature_17")] <- sapply(trainData[, c("feature_1","feature_2","feature_3","feature_17")], as.character)
trainData$feature_1 <- as.numeric(paste("0x", trainData$feature_1, sep=""))
trainData$feature_2 <- as.numeric(paste("0x", trainData$feature_2, sep=""))
trainData$feature_3 <- as.numeric(paste("0x", trainData$feature_3, sep=""))
trainData$feature_17 <- as.numeric(paste("0x", trainData$feature_17, sep=""))
trainData$id <- NULL
trainData$feature_0[is.na(trainData$feature_0)] <- mean(c(na.omit(trainData$feature_0)))
trainData$feature_10[is.na(trainData$feature_10)] <- mean(c(na.omit(trainData$feature_10)))
trainData$feature_16[is.na(trainData$feature_16)] <- mean(c(na.omit(trainData$feature_16)))

testData$feature_8 <- NULL
testData$feature_13 <- NULL
testData$feature_12 <- NULL
testData[,c("feature_1","feature_2","feature_3","feature_17")] <- sapply(testData[, c("feature_1","feature_2","feature_3","feature_17")], as.character)
testData$feature_1 <- as.numeric(paste("0x", testData$feature_1, sep=""))
testData$feature_2 <- as.numeric(paste("0x", testData$feature_2, sep=""))
testData$feature_3 <- as.numeric(paste("0x", testData$feature_3, sep=""))
testData$feature_17 <- as.numeric(paste("0x", testData$feature_17, sep=""))
testData$feature_0[is.na(testData$feature_0)] <- mean(c(na.omit(testData$feature_0)))
testData$feature_10[is.na(testData$feature_10)] <- mean(c(na.omit(testData$feature_10)))
testData$feature_16[is.na(testData$feature_16)] <- mean(c(na.omit(testData$feature_16)))


model <- svm(type="eps-regression", kernel="radial", target ~., data=trainData)
pred <- predict(model, testData, type="response")
result <- data.frame(id = testData$id, target = pred)
write.csv(result, file="results.csv")
