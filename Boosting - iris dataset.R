data("iris")
library(caret)
#Data partition for model building and testing
inTraininglocal<-createDataPartition(iris$Species,p=.70,list = F)
training<-iris[inTraininglocal,]
testing<-iris[-inTraininglocal,]

library(C50)
#Model Building
model<-C5.0(training$Species~.,data = training,trials=20) #Trials- Boosting parameter
#Generate the model summary

summary(model)
#Predict for test data set
pred<-predict.C5.0(model,testing[,-5])
a<-table(testing$Species,pred)
sum(diag(a))/sum(a)
