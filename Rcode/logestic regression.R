data <- read.csv("~/Desktop/stats final pj/parkinsons.data.csv",sep = ",", header = T)

str(data)
set.seed(188)


library(caret)
library(MASS)
library(lattice)
library(ggplot2)
library(randomForest)
library("ROCR")

#convert response variable to factor

data$status <- as.factor(data$status)

# Splitting the data into train and test. The distribution of the response variable is maintained in both the test and train data sets


train.prop=0.80
trainIndex <- createDataPartition(data$status, p=train.prop, list=FALSE)
data_train <- data[ trainIndex,]
data_test <- data[-trainIndex,]

#### Finding correlated variables

corr <- cor(data[-c(1,18)])
corrDF <- expand.grid(col = names(data)[-c(1,18)], row = names(data)[-c(1,18)])
corrDF$correlation <- as.vector(corr)
levelplot(correlation ~ row+ col, corrDF,scales=list(x=list(cex=.4),y=list(cex=.5)))


## Logistic Regression with all the features available in the data

fullmod = glm(status ~ .,data = data_train[-1],family=binomial)

# StepWise Regression to remove non useful features. We do backwards regression here.

backwards = stepAIC(fullmod,trace=0)

# Predicting on test data
prd.test <- predict(backwards,newdata = data_test,type = "response")
prd.train <- predict(backwards,newdata = data_train,type = "response")

conf_matrix_test <- table(data_test$status,ifelse(prd.test > 0.5 ,1,0))
conf_matrix_train <- table(data_train$status,ifelse(prd.train > 0.5 ,1,0))


# Area under the curve

pred <- prediction(prd.test, data_test$status)
perf <- performance(pred,"tpr","fpr")
plot(perf,col="black",lty=3, lwd=3)
auc <- performance(pred,"auc")
auc

pred <- prediction(prd.train, data_train$status)
perf <- performance(pred,"tpr","fpr")
plot(perf,col="black",lty=3, lwd=3)
auc <- performance(pred,"auc")
auc






