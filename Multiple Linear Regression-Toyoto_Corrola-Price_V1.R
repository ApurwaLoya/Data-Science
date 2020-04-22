toyota1<-read.csv("/Volumes/Data/Course Content/DS content/Linear Regression/Toyoto_Corrola.csv")

#Scatter Plot Matrix:
pairs(toyota)
toyota<-toyota1[,-c(1,2,8)]

#Correlation Matrix:
cor(toyota)
#Regression Model and Summary
model.car<-lm(Price~.,data = toyota)
summary(model.car)

#Multi-colinearity
install.packages("car")
library(car)
car::vif(model.car)
##Subset selection
#library(MASS)
#stepAIC(model.car)

#Model Building

#Diagnostic Plots:
#Residual Plots, QQ-Plos, Std. Residuals vs Fitted
plot(model.car) 
#Residuals vs Regressors
library(car)
residualPlots(model.car)
#Added Variable Plots
avPlots(model.car)

#QQ plots of studentized residuals
qqPlot(model.car)
#Deletion Diagnostics
influenceIndexPlot(model.car) # Index Plots of the influence measures

####Iteration 1 
#Remove 77th observation
toyota2<-toyota[-c(222,961,602),]
toyota2['Age2']<-toyota2$Age_08_04*toyota2$Age_08_04

model.car1<-lm(Price~.,data = toyota2)
summary(model.car1)
car::vif(model.car1)
plot(model.car1) 
residualPlots(model.car1)
qqPlot(model.car1)
influenceIndexPlot(model.car1)