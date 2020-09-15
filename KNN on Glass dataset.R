library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(class)

Glass_Data<-read.csv("E:\\Data Science\\Assignments\\glass.csv")
Glass<-scale(Glass_Data[,1:9])
GD<-cbind(Glass,Glass_Data[10])
GD$Type <- as.factor(GD$Type)
View(GD)

set.seed(90)

sample <- sample.split(GD$Type,SplitRatio = 0.70)
train <- subset(GD,sample==TRUE)
test <- subset(GD,sample==FALSE)

Pred<- knn(train[1:9],test[1:9],train$Type,k=1)
confusionMatrix(Pred, test$Type)

#Achieved an accuracy of 75 %. Lets try different values of k 


Pred <- NULL
error.rate <- NULL

for (i in 1:10) {
  Pred <- knn(train[1:9],test[1:9],train$Type,k=i)
  error.rate[i] <- mean(Pred!=test$Type)
  
}

knn.error <- as.data.frame(cbind(k=1:10,error.type =error.rate))

ggplot(knn.error,aes(k,error.type))+ geom_point()+ geom_line() + scale_x_continuous(breaks=1:10)+ theme_bw() +
  xlab("Value of K") +
  ylab('Error')

#Plot reveals that error is lowest when k=1 and so our model is OK
