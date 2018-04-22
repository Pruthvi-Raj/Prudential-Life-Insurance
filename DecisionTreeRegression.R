setwd("C:\\Neola\\ADS\\Project1\\ProjectFiles")
dataset <- read.csv("Prudential_Cleaned.csv")

#dataset$Response <- as.factor(dataset$Response)
#contrasts(dataset$Response)

#Taking data and splitting it into train and test
sample_index = sample(49999, 10000)
testData = dataset[sample_index,]
trainData = dataset[-sample_index,]

#Applying Decision Tree Regression on TrainData
library(rpart)
fit <- rpart(Response~.,method="anova", data=trainData,
             control=rpart.control(minsplit = 50,cp = 0.01))

#Printing plots for train data
fit
summary(fit)
rpart.plot::rpart.plot(fit)
printcp(fit)


#Display cross validation results
par(mfrow=c(1,2))
rsq.rpart(fit)

#Predicting the Response for test data
Prediction <- predict(fit, testData[-65])

#Print the confusion matrix
class.pred <- table(Prediction,testData$Response)
class.pred

#Calculating accuracy and correlation
sum(diag(class.pred))/sum(class.pred) * 100
cor(Prediction,testData$Response)


#Print Prediction summary
summary(round(Prediction))
str(Prediction)

Prediction <- round(Prediction)

#Adding the prediction column and writing it to a file
predictedData <- data.frame(testData[-65],Prediction)
#predicted <- rpart(Response~.,data = predictedData,method = "class")
#plot(predicted)
#text(predicted)

write.csv(predictedData, file = "Result.csv", row.names = FALSE)


