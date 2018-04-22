
# Reading the dataset
data = read.csv('A:\\sem-2 - spring-2017\\Advance-Data-Science\\mid-term-project\\Prudential_Cleaned.csv',header = TRUE)
str(data)

#splitting the data to train and test
trainData <- data[1:40000,]
testData <- data[(40001:50000),]


#Applying linear regression to the train data set
fit_data = lm(Response~., data=trainData )
summary(fit_data)

#plotting the  Linear Regression data
plot(fit_data)

# predicting the response on test data for linear regression
pred <- predict(fit_data, testData[-65])
pred <- round(pred)
summary(pred)
plot(pred)


mat <- table(pred,testData$Response)
sum(diag(mat))/sum(mat) * 100

#including prediction column of linear Regression to the file
predictedData <- data.frame(testData[-65],pred)
write.csv(predictedData, file = "A:\\sem-2 - spring-2017\\Advance-Data-Science\\mid-term-project\\Result_linearRegression.csv", row.names = FALSE)


#applying Lasso Regression
library(glmnet)
x <- model.matrix(Response~.,trainData)[,-65]
y <- trainData$Response
grid = 10^seq(10,-2, length = 100)
lasso.mode = glmnet(x, y, alpha=1, lambda = grid)
summary(lasso.mode)
coef(lasso.mode)

fit_lasso <- lm(Response~Product_Info_3 + Product_Info_4  + Ins_Age +Ht  +Wt + BMI +
                  Employment_Info_1+Employment_Info_2 + Product_Info_3 + Product_Info_4 +
                  Medical_History_39._.3 + Medical_History_39._.1 + Medical_History_39._.2 +
                  Medical_History_40._.3 + Medical_History_40._.1 + Medical_History_40._.2 +
                  Medical_History_41._.3 + Medical_History_41._.1 + Medical_History_41._.2 +
                  Medical_History_39._.3 + Medical_History_39._.1 + Medical_History_39._.2 +
                  Medical_History_40._.3 + Medical_History_40._.1 + Medical_History_40._.2 +
                  Medical_History_41._.3 + Medical_History_41._.1 + Medical_History_41._.2 , data=trainData)
summary(fit_lasso)

#plotting the  Lasso Regression data
plot(fit_lasso)

# predicting the response on test data for lasso regression
pred_lasso <- predict(fit_lasso, testData[-65])
pred_lasso <- round(pred_lasso)
summary(pred_lasso)
plot(pred_lasso)


mat_lasso <- table(pred_lasso,testData$Response)
sum(diag(mat_lasso))/sum(mat_lasso) * 100

rm_lasso <- sqrt( mean( (testData$Response - pred_lasso)^2 , na.rm = TRUE ) )
rm_lasso
#backward elimination
#step(fit_data, data=trainData, direction="backward")


#including prediction column of Lasso Regression to the file
predictedData_lasso <- data.frame(testData[-65],pred_lasso)
write.csv(predictedData_lasso, file = "A:\\sem-2 - spring-2017\\Advance-Data-Science\\mid-term-project\\Result_lassoRegression.csv", row.names = FALSE)












