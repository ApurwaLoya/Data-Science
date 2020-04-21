MPG<-read.csv("E:\\Data Science\\Assignments\\Cars.csv")

#Scatter Plot Matrix:
pairs(MPG)


#Correlation Matrix:
cor(MPG)
#Regression Model and Summary
model.car<-lm(MPG~HP+VOL+SP+WT,data = MPG)
summary(model.car)
#########Experiment#####################
reg_vol<-lm(MPG~VOL,data = MPG)
summary(reg_vol)
reg_wt<-lm(MPG~WT,data = MPG)
summary(reg_wt)
reg_wt_vol<-lm(MPG~WT+VOL,data = MPG)
summary(reg_wt_vol)
##################

#Regression Model and Summary
model.car<-lm(MPG~.,data = MPG)
summary(model.car)
#Multi-colinearity
install.packages("car")
library(car)
car::vif(model.car)
##Subset selection
library(MASS)
stepAIC(model.car)

#Model Building
#Regression Model and Summary
model.car<-lm(MPG~.,data = MPG)
summary(model.car)
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
MPG1<-MPG[-77,]
model1<-lm(MPG~.,data = MPG1)
car::vif(model1)
plot(model1) 
residualPlots(model1)
qqPlot(model1)
influenceIndexPlot(model1)

#iteration2
MPG2<-MPG[-c(77,79),]
model2<-lm(MPG~.,data = MPG2)
car::vif(model2)
plot(model2) 
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)


#iteration3
MPG3<-MPG[-c(77,79,80),]
model3<-lm(MPG~.,data = MPG3)
car::vif(model3)
plot(model3) 
residualPlots(model3)
qqPlot(model3)
influenceIndexPlot(model3)

summary(model3.car)

#iteration4
MPG4<-MPG[-c(66,77,79,80),]
model4<-lm(MPG~.,data = MPG4)
car::vif(model4)
plot(model4) 
residualPlots(model4)
qqPlot(model4)
influenceIndexPlot(model4)

summary(model4)

#iteration5
MPG5<-MPG[-c(66,71,77,79,80),]
model5<-lm(MPG~.,data = MPG5)
car::vif(model5)
plot(model5) 
residualPlots(model5)
qqPlot(model5)
influenceIndexPlot(model5)

summary(model5)

#iteration6
MPG6<-MPG[-c(66,70,71,77,79,80),]
model6<-lm(MPG~.,data = MPG6)
car::vif(model6)
plot(model6) 
residualPlots(model6)
qqPlot(model6)
influenceIndexPlot(model6)

summary(model6)


#iteration7
MPG7<-MPG[-c(66,70,71,77,78,79,80),]
model7<-lm(MPG~.,data = MPG7)
car::vif(model7)
plot(model7) 
residualPlots(model7)
qqPlot(model7)
influenceIndexPlot(model7)

summary(model7)
