library(ggplot2)
library(car)

Delivery_time <- read.csv("E:\\Data Science\\Assignments\\delivery_time.csv")
View(Delivery_time)
cor(Delivery_time$Delivery.Time,Delivery_time$Sorting.Time) 
plot(Delivery_time$Delivery.Time~Delivery_time$Sorting.Time)

model1 <- lm(Delivery_time$Delivery.Time~Delivery_time$Sorting.Time)
summary(model1)
influencePlot(model = model1)
predict1 <- predict(object =model1,newdata = Delivery_time)
cor(Delivery_time$Delivery.Time,predict1)
sqrt(sum(model1$residuals^2)/nrow(Delivery_time))    

model2 <- lm(Delivery_time$Delivery.Time~poly(x = Delivery_time$Sorting.Time,degree = 2))
summary(model2)
predict2 <- predict(model2,newdata = Delivery_time)
cor(predict2,Delivery_time$Delivery.Time)
sqrt(sum(model2$residuals^2)/nrow(Delivery_time))

model3 <- lm(Delivery_time$Delivery.Time~poly(x = Delivery_time$Sorting.Time,degree = 5))
summary(model3)
sqrt(sum(model3$residuals^2)/nrow(Delivery_time))
predict3 <- predict(object = model3,newdata = Delivery_time)
cor(predict3,Delivery_time$Delivery.Time)

model4 <- lm(log(Delivery_time$Delivery.Time)~poly(Delivery_time$Sorting.Time,2))
summary(model4)
sqrt(sum(model4$residuals^2)/nrow(Delivery_time)) 
predict4 <-  predict(object = model4,newdata = Delivery_time)

pred4_act <- exp(predict4)
Delivery_time$Delivery.Time
sqrt(sum((Delivery_time$Delivery.Time-pred4_act)^2)/nrow(Delivery_time)) 
cor(exp(predict4),Delivery_time$Delivery.Time)
