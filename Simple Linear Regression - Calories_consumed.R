library(ggplot2)
library(car)

calories_consumed <- read.csv("E:\\Data Science\\Assignments\\calories_consumed.csv")
View(calories_consumed)
colnames(calories_consumed) <- c("WeightGain","CaloriesConsumed")

shapiro.test(calories_consumed$WeightGain)
shapiro.test(calories_consumed$CaloriesConsumed)

cor(calories_consumed$WeightGain,calories_consumed$CaloriesConsumed)
plot(calories_consumed$WeightGain~calories_consumed$CaloriesConsumed)

model1 <- lm(calories_consumed$WeightGain~calories_consumed$CaloriesConsumed)
summary(model1) 
predict1 <- predict(object =model1,newdata = calories_consumed)
cor(calories_consumed$WeightGain,predict1) 
sqrt(sum(model1$residuals^2)/nrow(calories_consumed))  
plot(x = calories_consumed$CaloriesConsumed,y = predict1,type = "b",col="red",xlab = "Calories consumed",ylab = "Weight Gain")
points(x = calories_consumed$CaloriesConsumed,y = calories_consumed$WeightGain)
ggplot(data = calories_consumed,mapping=aes(x = CaloriesConsumed,y=WeightGain))+geom_smooth(method = "lm")+geom_point()+ggtitle("Model 1",subtitle = "Y~X")

model2 <- lm(calories_consumed$WeightGain~poly(x = calories_consumed$CaloriesConsumed,degree = 2))
summary(model2) 
predict2 <- predict(model2,newdata = calories_consumed)
cor(predict2,calories_consumed$WeightGain) 
sqrt(sum(model2$residuals^2)/nrow(calories_consumed))  
ggplot(data = calories_consumed,mapping=aes(x = CaloriesConsumed,y=WeightGain))+geom_smooth(method = "lm",formula = y~poly(x,2))+geom_point()+ggtitle("Model 2",subtitle = "Second degree polynomial")
