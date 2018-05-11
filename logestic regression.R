data <- read.csv("~/Desktop/stats final pj/parkinsons.data.csv",sep = ",", header = T)

str(data)
set.seed(2018)


library(caret)
library(MASS)
library(lattice)
library(ggplot2)
library(randomForest)
library("ROCR")
library(corrgram)


#convert response variable to factor

data$status <- as.factor(data$status)

# Splitting the data into train and test.

trainIndex <- createDataPartition(data$status, p=0.8, list=FALSE)
data_train <- data[ trainIndex,]
data_test <- data[-trainIndex,]

## Logistic Regression with all the features available in the data

fullmod = glm(status ~ .,data = data_train[-1],family=binomial)


# Predicting on test data
prd.test <- predict(fullmod,newdata = data_test,type = "response")
prd.train <- predict(fullmods,newdata = data_train,type = "response")

library(pROC)


#Get the value of accuracy, precision, recall and f1.
p_lr = precision(conf_matrix_test)
r_lr = recall(conf_matrix_test)
f_lr = (2 * p_lr * r_lr) / (p_lr + r_lr)
confusionMatrix(conf_matrix_test, positive = "1")

#Get the plot of AUC(ROC)
modelroc <- roc(ifelse(prd.train > 0.5 ,1,0),as.numeric(data_train$status))
plot(modelroc,print.auc = TRUE, auc.polygon  = TRUE, max.auc.polygon = TRUE,auc.polygon.col = 'skyblue', main = 'ROC curve of Logestic Regression')



