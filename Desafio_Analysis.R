library(data.table)
library(sm)
library(DAAG)
library(mice)
library(caret)
library(e1071)

setwd("Desktop/Desafio VivaReal/")

trainData <- read.csv("challenge_datasets/train.csv")

par(mfrow=c(4,5))

#---------------- Draw Plots of Relations Between Features and Target ---------------------
for(feature in colnames(trainData[2:21]))
  plot(trainData$target, trainData[,feature], ylab = feature, xlab = "Target");


#From this visual analysis it is possible to see that some features have 
# similar distributions and patterns, therefore making it useless to use all of 
# them

#We will first see the correlation between the numeric features and see if any of them are highly
#correlated and therefore not needed.
nums <- sapply(trainData, is.numeric)
correlationMatrix <- cor(trainData[,nums])
print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
print(highlyCorrelated)

#Feature8 has a correlation of over 0.99 with feature 11, therefore we can remove feature 8
trainData$feature_8 <- NULL

#Feature 13 is the same for all items, therefore I'm removing it since it is useless
trainData$feature_13 <- NULL

#By looking at features 12 and 14, they seemed to have similar numbers so I'll look into that
df <- data.frame(dif = trainData$feature_12 - trainData$feature_14)
length(df$dif[df$dif!=0 & !is.na(df$dif)])/length(df$dif)
#Only 3 percent of them are different so I'll delete one of them
trainData$feature_12 <- NULL

#The features 1,2,3 and 17 are considered for now as factors but each has more than 10k different levels,
# which makes me think that they are not really factors but actually numbers that can be converted from hex.
trainData[,c("feature_1","feature_2","feature_3","feature_17")] <- sapply(trainData[, c("feature_1","feature_2","feature_3","feature_17")], as.character)
trainData$feature_1 <- as.numeric(paste("0x", trainData$feature_1, sep=""))
trainData$feature_2 <- as.numeric(paste("0x", trainData$feature_2, sep=""))
trainData$feature_3 <- as.numeric(paste("0x", trainData$feature_3, sep=""))
trainData$feature_17 <- as.numeric(paste("0x", trainData$feature_17, sep=""))

#Last thing is to remove the ID
trainData$id <- NULL

#Clean Memory
rm(feature)
rm(nums)
rm(correlationMatrix)
rm(highlyCorrelated)
rm(df)
gc()


#Cross Validation Function
cross_val <- function(data, n, mode, modelType, verbose=FALSE){
  
  #Randomly shuffle the data
  data<-data[sample(nrow(data)),]
  #Create n equally size folds
  folds <- cut(seq(1,nrow(data)),breaks=n,labels=FALSE)
  
  
  #Perform n fold cross validation
  errors <- c()
  for(i in 1:n){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    test <- data[testIndexes, ]
    train <- data[-testIndexes, ]
    if(mode == "column"){
      train$feature_0 = NULL
      train$feature_10 = NULL
      train$feature_16 = NULL
    }
    else if(mode == "average"){
      train$feature_0[is.na(train$feature_0)] <- mean(c(na.omit(train$feature_0)))
      train$feature_10[is.na(train$feature_10)] <- mean(c(na.omit(train$feature_10)))
      train$feature_16[is.na(train$feature_16)] <- mean(c(na.omit(train$feature_16)))
      
      test$feature_0[is.na(test$feature_0)] <- mean(c(na.omit(test$feature_0)))
      test$feature_10[is.na(test$feature_10)] <- mean(c(na.omit(test$feature_10)))
      test$feature_16[is.na(test$feature_16)] <- mean(c(na.omit(test$feature_16)))
    }
    else if(mode == "zero"){
      train$feature_0[is.na(train$feature_0)] <- 0
      train$feature_10[is.na(train$feature_10)] <- 0
      train$feature_16[is.na(train$feature_16)] <- 0
      test$feature_0[is.na(test$feature_0)] <- 0
      test$feature_10[is.na(test$feature_10)] <- 0
      test$feature_16[is.na(test$feature_16)] <- 0
    }
    if(verbose)
        print(paste("Starting ", modelType, " regression calculations."))
    if(modelType == "linear_regression")
      model <- lm(target~., data=train)
    else if(modelType == "SVM_Linear")
      model <- svm(type="eps-regression", kernel="linear", target ~., data=train)
    else if(modelType == "SVM_Polynomial")
      model <- svm(type="eps-regression", kernel="polynomial", target ~., data=train)
    else if(modelType == "SVM_Radial")
      model <- svm(type="eps-regression", kernel="radial", target ~., data=train)
    else if(modelType == "SVM_Sigmoid")
      model <- svm(type="eps-regression", kernel="sigmoid", target ~., data=train)
    
    if(verbose)
      print("Starting prediction")
    pred <- predict(model, test, type="response")
    errors <- c(errors, median( abs(test$target - pred) / pred ))
    if(verbose)
      print(paste("Error round ", i,": ",median( abs(test$target - pred) / pred )))
  }
  if(verbose)
    print(paste("AVG Error: ", mean(errors)))
  return(mean(errors))
}

#Analysis of missing values on each column
md.pattern(trainData)

#We get from this analysis that features 0, 10 and 16 have a significant number of missing values.
#Now I'll test different methods to handle those cases and see which one has a better result.


bestMode <- ""
bestModel <- ""
bestError <- 999

#column - Ignore Columns with NA
#average -  Add average column values where there is NA
#zero - Add zero to where there is NA
modes <- c("column", "average", "zero")


models <- c("linear_regression", "SVM_Linear", "SVM_Polynomial", "SVM_Radial", "SVM_Sigmoid")
for(md in modes){
  for(ml in models){
    error <-  cross_val(trainData, 5, md, ml, verbose=TRUE)
    if(bestError > error){
      bestError = error
      bestMode = md
      bestModel = ml
    }
  }
}

print(paste("The best model was:", bestModel, ", using the mode: ", bestMode, " with error: ", bestError))   
#"The best model was: SVM_Radial , using the mode:  average  with error:  0.0635724920681937"




