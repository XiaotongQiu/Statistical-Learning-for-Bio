## Tree

data <- read.csv("~/Desktop/stats final pj/parkinsons.data.csv",sep = ",", header = T)

str(data)
set.seed(2018)

library(caret)
library(MASS)
library(lattice)
library(ggplot2)
library(randomForest)
library(tree)

#convert response variable to factor

data$status <- as.factor(data$status)

# Splitting the data into train and test. The distribution of the response variable is maintained in both the test and train data sets


train.prop=0.50
trainIndex <- createDataPartition(data$status, p=train.prop, list=FALSE)
data_train <- data[ trainIndex,]
data_test <- data[-trainIndex,]

tree1.tree = tree(formula = status ~ ., data = data_train[-1])
plot(tree1.tree)
text(tree1.tree)

cv.tr = cv.tree(tree1.tree)

t1.pruned = prune.tree(tree1.tree, best = cv.tr$size[which.min(cv.tr$misclass)])
plot(t1.pruned)
text(t1.pruned)

#决策树性能分析
p = predict(tree1.tree,data_test,type = "class")
tree.table = table(data_test$status, p, dnn = c("Actual","Predicted"))

p_tree = precision(tree.table)
r_tree = recall(tree.table)
f_tree = (2 * p_tree * r_tree) / (p_tree + r_tree)
confusionMatrix(tree.table, positive = "1")

777

