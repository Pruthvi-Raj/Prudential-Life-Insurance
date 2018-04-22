# loading neccessary packages and dataset
install.packages("caret")
install.packages("e1071")
install.packages("Lahman")
install.packages("plyr")
library(caret)
library(e1071)
library(Lahman)
library(dplyr)

library(randomForest)
dataset <- read.csv('C:\\Users\\Pruth\\Desktop\\Prudential_Cleaned.csv',header = TRUE)
dataset

#Initially the data is sampled i.e., divided as 200 and 800 samples as we can see that out of
#59000 samples 10000 are taken initially and in the next step we can observe that this sample is
#assigned as test_dataset and eventually the remaining 800 values are assigned as train_dataset
sample_index = sample(500, 100)
test_dateset = dataset[sample_index,]
sample_index1 = sample(1000, 200)
train_dateset = dataset[sample_index1,]
test_dateset
train_dateset


#svm.fit<- svm(Response~., data=train_dateset,scale = TRUE)
#summary(svm.fit)



#predict.svm<- predict(svm.fit,test_dateset)


rf<-randomForest(Response~.,data=train_dateset,importance=TRUE,ntree=100)

imp<- importance(rf, type=1)
imp
imp<-data.frame(predictors=rownames(imp), imp)

imp.sort<- arrange(imp,desc(X.IncMSE))
imp.sort$predictors <- factor(imp.sort$predictors, level= imp.sort$predictors)


imp.20<- imp.sort[1:20,]
print(imp.20)



#SVM Parameters are obtained which best suit the model  
#gamma & cost values are decided based on the tune values
#substituted in the model
tuned <-tune.svm(Response~Medical_Keyword_3+BMI+Medical_History_28._.1+Medical_History_28._.2+Insurance_History_2._.1+Insurance_History_2._.3+Wt+Medical_History_39._.1+Medical_History_1+Product_Info_4+Medical_Keyword_34+ InsuredInfo_5._.1, data = train_dateset, gamma = 10^(-1:1), cost = 10^(.5:1))
summary(tuned)


#The model is trained with specific parameters of cost and gamma and train data and later based 
#on the best performance while substituting different kernels we decide the best kernel suitable 
#which has the maximum accuracy
model = svm(Response ~. , kernel = "radial", cost =10, gamma =0.1, data = train_dateset, scale = F)


#After the model is trained using the train_dateset the next step is to predict the values for 
#the test sample using the predicted values and the test sample
#Both the model and test_dateset sample with only the required column are given as input to 
#the prediction function
predictions <-  predict(model, test_dateset[-65])
predictions
newpred<-round(predictions)
newpred
matpred<-as.matrix(newpred)
matpred
test_dateset[65]

#The predicted values and the test sample are compared and a confusion matrix is obtained
#using this confusion matrix we calculate the accuracy of the model
xtable<-table(test_dateset[,65], newpred)
xtable
#x<-(matpred-(test_dateset[,65]))
#sum(diag(xtable))/sum(xtable)
#diag(xtable)

#Once the table is obtained we sum up the True negative and True positive values and divide that 
#by the total number of values predicted which gives the accuracy and based on this accuracy we 
#decide which kernel to select
#mean((matpred-(test_dateset[,65]))^2)

library(caret)
library(ggplot2)

confusionMatrix(test_dateset[,65], matpred)

