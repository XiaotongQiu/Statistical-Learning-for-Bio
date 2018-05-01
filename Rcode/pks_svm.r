# Dataset available: https://archive.ics.uci.edu/ml/datasets/parkinsons
# Label col: status
# Feature cols: all other columns except names

# Read dataset
data = read.csv("~/Desktop/parkinsons.data.csv")
# Drop names
cordata<-data[,-c(1)]
# Sampling; Create training set and test set
sample <- sample.int(n = nrow(cordata), size = floor(.75*nrow(cordata)), replace = F)
train <- cordata[sample, ]
test  <- cordata[-sample, ]

################ Some Plots ################
library(ggplot2)
# pie chart of patient status
cordata$status<-as.factor(cordata$status)
#test$status<-as.factor(test$status)
#train$status<-as.factor(train$status)
#pie<-ggplot(train, aes(x=factor(1)), fill=status)+
#  geom_bar()+
#  coord_polar("y")+scale_x_discrete("")

#ggplot(train) +
#geom_bar( aes(status) )

#qplot(spread1,status,data=train,color=status)

# library(corrplot)
# x <- sapply(train, cor, y=train$status)
# x<- data.frame(x)
# corrplot(as.matrix(x))
# ggplot(x)+coord_polar("y")
# 
# qplot(x,data=x)
################################################

# Support Vector Machine with different kernels
library(e1071)
for (i in c('linear','polynomial','radial','sigmoid'))
# traing
svm <- svm(status~.,data=cordata,kernel=i)
train_svm <-predict(svm,train)
tab <- table(predicted=train_svm,actual=train$status)
print('Prediction vs Actual(Training)')
print(tab)
acc_train <- sum(diag(tab))/sum(tab)
cat('Training Accuracy using',i,'kernel is:',acc_train,'\n')
# test
test_svm <- predict(svm,test)
tab2 <- table(predicted=test_svm_linear,actual=test$status)
print('Prediction vs Actual(Test)')
print(tab2)
acc_test <- sum(diag(tab2))/sum(tab2)
cat('Test Accuracy using',i,'kernel is:',acc_test,'\n')



# #tuning
# set.seed(123)
# #tmodel <-tune(svm,status~.,data=train,ranges=list(epsilon=seq(0,1,0.1),cost = 2^(2:9)))
# #plot(tmodel)

#best model
#mymodel <- tmodel$best.model
# pred2<-predict(mymodel,test)
# tab2<-table(predicted=pred2,actual=test$status)
# classification_rate2<-sum(diag(tab2))/sum(tab2)
# classification_rate2
