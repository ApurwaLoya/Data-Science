Price<-read.csv("E:\\Data Science\\Class\\Toyoto_Corrola.csv")
View(Price)
#Scatter Plot Matrix:
pairs(Price)


#Correlation Matrix:
cor(Price[,3:10])
#Regression Model and Summary
model.Price<-lm(Price~Age+KM+HP+Doors+Cylinders+Gears+Weight,data = Price)
summary(model.Price)

#########Experiment#####################

model1<-lm(Price~Age+KM+HP+Gears+Weight,data = Price)
summary(model1)

reg_D<-lm(Price~Doors,data = Price)
summary(reg_D)

reg_C<-lm(Price~Cylinders,data = Price)
summary(reg_C)

reg_D_C<-lm(Price~Doors+Cylinders,data = Price)
summary(reg_D_C)
##################

#Regression Model and Summary
model.Toyoto<-lm(Price~Age+KM+HP+Doors+Cylinders+Gears+Weight,data = Price)
summary(model.Toyoto)
vif(model.Toyoto)
alias(lm(Price~Age+KM+HP+Doors+Cylinders+Gears+Weight,data = Price))

#Multi-colinearity
model.Toyoto<-lm(Price~Age+KM+HP+Doors+Gears+Weight,data = Price)
summary(model.Toyoto)
vif(model.Toyoto)

##Subset selection

stepAIC(model.Toyoto)

#Model Building
#Regression Model and Summary
model.Toyoto<-lm(Price~Age+KM+HP+Doors+Gears+Weight,data = Price)
summary(model.Toyoto)

#Diagnostic Plots:
#Residual Plots, QQ-Plos, Std. Residuals vs Fitted
plot(model.Toyoto)

#Residuals vs Regressors

residualPlots(model.Toyoto)
#Added Variable Plots
avPlots(model.Toyoto)
#QQ plots of studentized residuals
qqPlot(model.Toyoto)
#Deletion Diagnostics
influenceIndexPlot(model.Toyoto) # Index Plots of the influence measures

####Iteration 1 
#Remove 222th observation
Price1<-Price[-222,]
model1<-lm(Price~Age+KM+HP+Doors+Gears+Weight,data = Price1)
vif(model1)
plot(model1) 
residualPlots(model1)
qqPlot(model1)
influenceIndexPlot(model1)
summary(model1)
#iteration2
Price2<-Price[-c(222,961),]
model2<-lm(Price~Age+KM+HP+Doors+Gears+Weight , data = Price2)
summary(model2)
vif(model2)
plot(model2) 
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)


#iteration3
Price3<-Price[-c(222,602,961),]
model3<-lm(Price~Age+KM+HP+Doors+Gears+Weight , data = Price3)
summary(model3)
vif(model3)
plot(model3) 
residualPlots(model3)
qqPlot(model3)
influenceIndexPlot(model3)

