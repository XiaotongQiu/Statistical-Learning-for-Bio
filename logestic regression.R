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

# Splitting the data into train and test. The distribution of the response variable is maintained in both the test and train data sets

trainIndex <- createDataPartition(data$status, p=0.8, list=FALSE)
data_train <- data[ trainIndex,]
data_test <- data[-trainIndex,]

#### Finding correlated variables

corr <- cor(data[-c(1,18)])
corrDF <- expand.grid(col = names(data)[-c(1,18)], row = names(data)[-c(1,18)])
corrDF$correlation <- as.vector(corr)
levelplot(correlation ~ row+ col, corrDF,main = 'Correlation among variables', scales=list(x=list(cex=.4),y=list(cex=.6)))
corrgram(data_train, order=NULL, main="Correlogram",  col.regions=colorRampPalette(c("darkgoldenrod4", "burlywood1",
"darkkhaki", "darkgreen"))) 
## Logistic Regression with all the features available in the data

fullmod = glm(status ~ .,data = data_train[-1],family=binomial)

# StepWise Regression to remove non useful features. We do backwards regression here.

backwards = stepAIC(fullmod, trace = FALSE)

# Predicting on test data
prd.test <- predict(backwards,newdata = data_test,type = "response")
prd.train <- predict(backwards,newdata = data_train,type = "response")

conf_matrix_test <- table(data_test$status,ifelse(prd.test > 0.5 ,1,0))
conf_matrix_train <- table(data_train$status,ifelse(prd.train > 0.5 ,1,0))

library(pROC)


#ROC
# roc(ifelse(prd.train > 0.5 ,1,0),as.numeric(data_train$status), plot = TRUE,smooth = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)
modelroc <- roc(ifelse(prd.train > 0.5 ,1,0),as.numeric(data_train$status))
plot(modelroc,print.auc = TRUE, auc.polygon  = TRUE, max.auc.polygon = TRUE,auc.polygon.col = 'skyblue')


p_lr = precision(conf_matrix_test)
r_lr = recall(conf_matrix_test)
f_lr = (2 * p_lr * r_lr) / (p_lr + r_lr)
confusionMatrix(conf_matrix_test, positive = "1")
confusionMatrix(conf_matrix_train, positive = "1")

666


