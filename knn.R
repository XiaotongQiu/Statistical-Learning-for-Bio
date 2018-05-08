##KNN


data <- read.csv("~/Desktop/stats final pj/parkinsons.data.csv",sep = ",", header = T)

str(data)


set.seed(188)

library(caret)
library(MASS)
library(randomForest)
library(kknn)


#convert response variable to factor

data$status <- as.factor(data$status)

# Splitting the data into train and test. The distribution of the response variable is maintained in both the test and train data sets
train.prop=0.80
trainIndex <- createDataPartition(data$status, p=train.prop, list=FALSE)
data_train <- data[ trainIndex,]
data_test <- data[-trainIndex,]

#KNN
pks.kknn <- kknn(status~., data_train, data_test, distance = 1, kernel = "triangular")
summary(pks.kknn)  

# 获取fitted.values  
fit <- fitted(pks.kknn)  

# 建立表格检验判类准确性  
table(data_test$status, fit, dnn = c("Actual","Predicted"))  

#ROC
roc(fit,as.numeric(data_test$status), smooth = TRUE, plot = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)



