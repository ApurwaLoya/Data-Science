library(ggplot2)
library(car)

Emp_data<- read.csv("E:\\Data Science\\Assignments\\emp_data.csv")
View(Emp_data)
cor(Emp_data$Salary_hike,Emp_data$Churn_out_rate) 
plot(Emp_data$Salary_hike,Emp_data$Churn_out_rate)

model1 <- lm(Emp_data$Churn_out_rate~Emp_data$Salary_hike)
summary(model1)
predict1 <- predict(object =model1,newdata = Emp_data)
cor(Emp_data$Churn_out_rate,predict1)
sqrt(sum(model1$residuals^2)/nrow(Emp_data))

model2 <- lm(Emp_data$Churn_out_rate~poly(x = Emp_data$Salary_hike,degree = 2))
summary(model2)
predict2 <- predict(model2,newdata = Emp_data)
cor(predict2,Emp_data$Churn_out_rate)
sqrt(sum(model2$residuals^2)/nrow(Emp_data))

plot(x = Emp_data$Salary_hike,y = predict2,type = "b",col="red",xlab = "Calories consumed",ylab = "Weight Gain")
points(x = Emp_data$Salary_hike,y = Emp_data$Churn_out_rate)
