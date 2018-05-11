# Read dataset
data = read.csv("~/Desktop/stats final pj/parkinsons.data.csv")


library(caret)
library(MASS)
library(lattice)
library(ggplot2)
library(randomForest)
library("ROCR")
library(corrgram)
library(pROC)
set.seed(2018)


#convert response variable to factor

data$status <- as.factor(data$status)

# Splitting the data into train and test.

trainIndex <- createDataPartition(data$status, p=0.8, list=FALSE)
data_train <- data[ trainIndex,]
data_test <- data[-trainIndex,]


# Support Vector Machine with different kernels
library(e1071)
for (i in c('linear','polynomial','radial','sigmoid'))
  # traing
svm <- svm(status~.,data=cordata,kernel= 'radial')
train_svm <-predict(svm,data_train)
tab <- table(predicted=train_svm,actual=data_train$status)
print('Prediction vs Actual(Training)')
print(tab)
acc_train <- sum(diag(tab))/sum(tab)
#cat('Training Accuracy using',i,'kernel is:',acc_train,'\n')
# test
test_svm <- predict(svm,data_test)
tab2 <- table(predicted= test_svm,actual=data_test$status)
print('Prediction vs Actual(Test)')
print(tab2)
#cat('Test Accuracy using',i,'kernel is:',acc_test,'\n')


#Get the value of accuracy, precision, recall and f1.
acc_svm <- sum(diag(tab2))/sum(tab2)
p_svm = precision(tab2)
r_svm = recall(tab2)
f_svm = (2 * p_svm * r_svm) / (p_svm + r_svm)

#Get the plot of AUC(ROC)
roc(test_svm,as.numeric(data_test$status), main = 'ROC curve of SVM(linear)', auc.polygon.col = 'skyblue', plot = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE, 
    grid = TRUE, print.auc = TRUE)
