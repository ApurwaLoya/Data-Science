data("iris")
View(iris)

iris$Species<-as.factor(iris$Species) 
summary(iris)
library(party)
library(caret)

set.seed(7)
inTraininglocal<-createDataPartition(iris$Species,p=.70,list = F)
training<-iris[inTraininglocal,]
testing<-iris[-inTraininglocal,]

#Model Building

colnames(iris)
tree_training<-ctree(Species~Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=training)
plot(tree_training)
 
summary(tree_training)

#Predict for test data set

test_predict <- predict(tree_training, newdata= testing,type="response")
table(test_predict, testing$Species)

a<-table(test_predict, testing$Species)
sum(diag(a))/sum(a)

plot(tree_training)
print(tree_training)
plot(tree_training, type="simple")
