###Random Forest

data <- read.csv("~/Desktop/stats final pj/parkinsons.data.csv",sep = ",", header = T)

str(data)
set.seed(2019)

library(caret)
library(MASS)
library(lattice)
library(ggplot2)
library(randomForest)

#convert response variable to factor

data$status <- as.factor(data$status)

# Splitting the data into train and test. The distribution of the response variable is maintained in both the test and train data sets


train.prop=0.80
trainIndex <- createDataPartition(data$status, p=train.prop, list=FALSE)
data_train <- data[ trainIndex,]
data_test <- data[-trainIndex,]

control <- trainControl(method="repeatedcv", number=10, repeats=3, summaryFunction = twoClassSummary,classProbs = TRUE)
set.seed(1234)
tunegrid <- expand.grid(.mtry=c(1:10))
rf_gridsearch <- randomForest(data_train$status ~., data=data_train[-c(1,18)])
print(rf_gridsearch)
plot(rf_gridsearch)

pred.rf.test <- predict(rf_gridsearch,newdata = data_test[-c(1,18)])
conf_matrix_test_rf <- table(data_test$status,pred.rf.test)

pred.rf.train <- predict(rf_gridsearch,newdata = data_train[-c(1,18)])
conf_matrix_train_rf <- table(data_train$status,pred.rf.train)

p_rf = precision(conf_matrix_test_rf)
r_rf = recall(conf_matrix_test_rf)
f_rf = (2 * p_rf * r_rf) / (p_rf + r_rf)
confusionMatrix(conf_matrix_test_rf, positive = "1")

library(pROC)
# pred.rf.test = as.numeric(pred.rf.test)
# mode(pred.rf.test)
roc(pred.rf.train,as.numeric(data_train$status), plot = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)
