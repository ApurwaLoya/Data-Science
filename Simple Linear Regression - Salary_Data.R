library(ggplot2)
library(car)

Salary_Data <- read.csv("E:\\Data Science\\Assignments\\Salary_Data.csv")
View(Salary_Data)
plot(Salary_Data$YearsExperience,Salary_Data$Salary)
boxplot(Salary_Data)
hist(Salary_Data$YearsExperience)
hist(Salary_Data$Salary)
summary(Salary_Data)

Model1<-lm(Salary_Data$Salary~Salary_Data$YearsExperience)
summary(Model1)

Model2<-lm(Salary_Data$Salary~log(Salary_Data$YearsExperience))
summary(Model2)

Model3<-lm(log(Salary_Data$Salary)~Salary_Data$YearsExperience)
summary(Model3)

Model4<-lm(Salary_Data$Salary~poly(x=Salary_Data$YearsExperience, degree=2))
summary(Model4)

predict <- predict(Model4,newdata = Salary_Data)
cor(predict,Salary_Data$Salary) 
sqrt(sum(Model4$residuals^2)/nrow(Salary_Data))  
plot(Model4)

