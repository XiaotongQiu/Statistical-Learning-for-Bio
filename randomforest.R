###Random Forest

data <- read.csv("~/Desktop/stats final pj/parkinsons.data.csv",sep = ",", header = T)

str(data)
set.seed(2018)

library(caret)
library(MASS)
library(lattice)
library(ggplot2)
library(randomForest)

#convert response variable to factor

data$status <- as.factor(data$status)

# Splitting the data into train and test.

trainIndex <- createDataPartition(data$status, p=0.8, list=FALSE)
data_train <- data[ trainIndex,]
data_test <- data[-trainIndex,]


pred.rf.test <- predict(rf_gridsearch,newdata = data_test[-c(1,18)])
conf_matrix_test_rf <- table(data_test$status,pred.rf.test)

pred.rf.train <- predict(rf_gridsearch,newdata = data_train[-c(1,18)])
conf_matrix_train_rf <- table(data_train$status,pred.rf.train)

#Get the value of accuracy, precision, recall and f1.
p_rf = precision(conf_matrix_test_rf)
r_rf = recall(conf_matrix_test_rf)
f_rf = (2 * p_rf * r_rf) / (p_rf + r_rf)
confusionMatrix(conf_matrix_test_rf, positive = "1")

library(pROC)
#Get the plot of AUC(ROC)
roc(pred.rf.test,as.numeric(data_test$status), main = 'ROC curve of Random Forest', auc.polygon.col = 'skyblue', plot = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)
