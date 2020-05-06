data("iris")

library(ipred)
set.seed(300)
train<-iris[1:130,]
test<-iris[131:150,]
mybag <- bagging(train$Species ~ ., data = train, nbagg = 50)
credit_pred <- predict(mybag, test[,-5])
table(credit_pred, test$Species)
