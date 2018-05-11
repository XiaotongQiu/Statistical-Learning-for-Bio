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

# Splitting the data into train and test.

trainIndex <- createDataPartition(data$status, p=0.8, list=FALSE)
data_train <- data[ trainIndex,]
data_test <- data[-trainIndex,]


#Build the tree
tree1.tree = tree(formula = status ~ ., data = data_train[-1])
plot(tree1.tree)
text(tree1.tree)

cv.tr = cv.tree(tree1.tree)

# Tree pruning
t1.pruned = prune.tree(tree1.tree, best = cv.tr$size[which.min(cv.tr$misclass)])
plot(t1.pruned)
text(t1.pruned)

#Performance of decision tree
p = predict(tree1.tree,data_test,type = "class")
tree.table = table(data_test$status, p, dnn = c("Actual","Predicted"))

#Get the value of accuracy, precision, recall and f1.
p_tree = precision(tree.table)
r_tree = recall(tree.table)
f_tree = (2 * p_tree * r_tree) / (p_tree + r_tree)
confusionMatrix(tree.table, positive = "1")

#Get the plot of AUC(ROC)
roc(p,as.numeric(data_test$status), main = 'ROC curve of Decision Tree', auc.polygon.col = 'skyblue', plot = TRUE, auc.polygon = TRUE, 
    max.auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)
