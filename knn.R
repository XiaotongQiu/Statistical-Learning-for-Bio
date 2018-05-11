##KNN


data <- read.csv("~/Desktop/stats final pj/parkinsons.data.csv",sep = ",", header = T)

str(data)


set.seed(2018)

library(caret)
library(MASS)
library(randomForest)
library(kknn)


#convert response variable to factor

data$status <- as.factor(data$status)

# Splitting the data into train and test.

trainIndex <- createDataPartition(data$status, p=0.8, list=FALSE)
data_train <- data[ trainIndex,]
data_test <- data[-trainIndex,]


#KNN
pks.kknn <- kknn(status~., data_train, data_test, distance = 1, kernel = "triangular")
summary(pks.kknn)  

# Get the valur of fitted.values  
fit <- fitted(pks.kknn)  

# Build the table to testify the accuracy 
knn_table = table(data_test$status, fit, dnn = c("Actual","Predicted"))  



#Get the value of accuracy, precision, recall and f1.
p_knn = precision(knn_table)
r_knn = recall(knn_table)
f_knn = (2 * p_knn * r_knn) / (p_knn + r_knn)
confusionMatrix(knn_table, positive = "1")

#Get the plot of AUC(ROC)
roc(fit,as.numeric(data_test$status), main = 'ROC curve of KNN', auc.polygon.col = 'skyblue', plot = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)



