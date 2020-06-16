CD<-read.csv("E:\\Data Science\\Assignments\\Computer_Data.csv")
View(CD)
class(CD)
Computer_data<-CD
Computer_data$cd <- as.numeric(revalue(Computer_data$cd,c("yes"=1, "no"=0)))
Computer_data$multi <- as.numeric(revalue(Computer_data$multi,c("yes"=1, "no"=0)))
Computer_data$premium <- as.numeric(revalue(Computer_data$premium,c("yes"=1, "no"=0)))
View(Computer_data)
class(Computer_data)

attach(Computer_data)
summary(Computer_data)
plot(speed,price)
plot(hd,price)
plot(ram,price)
plot(screen,price)
plot(cd,price)
plot(multi,price)
plot(premium,price)
plot(ads,price)
plot(trend,price)
windows();pairs(Computer_data)
cor(Computer_data)

Model.Computer_data <- lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend)
summary(Model.Computer_data) 

library(car)
car::vif(Model.Computer_data)
library(MASS)
stepAIC(Model.Computer_data)

plot(Model.Computer_data)
residualPlots(Model.Computer_data)
avPlots(Model.Computer_data)
qqPlot(Model.Computer_data)

influenceIndexPlot(Model.Computer_data)


Model.Computer_dataLog <- lm(price~log(speed)+log(hd)+log(ram)+log(screen)+
                               log(cd)+log(multi)+log(premium)+log(ads)+log(trend)
                             ,data=Computer_data)
summary(Model.Computer_dataLog) 

Model.Computer_dataLog1 <- lm(price~log(speed)+log(hd)+log(ram)+log(screen)+
                               log(cd)+log(multi)+log(premium)+log(ads)+log(trend)
                             ,data=Computer_data[-c(1441,1701),])
summary(Model.Computer_dataLog1) 


Model.Computer_exp<-lm(log(price)~speed+hd+ram+screen+cd+multi+premium+ads+trend,
                       data=Computer_data[-c(1441,1701),])
summary(Model.Computer_exp)

Model.Computer_Quad <- lm(price~speed+I(speed^2)+hd+I(hd^2)+ram+I(ram^2)+screen+I(screen^2)+
                            +cd+I(cd^2)+multi+I(multi^2)+premium+I(premium^2)
                          +ads+I(ads^2)+trend+I(trend^2),data=Computer_data[-c(1441,1701),])
summary(Model.Computer_Quad)

plot(Model.Computer_Quad)
residualPlots(Model.Computer_Quad)
avPlots(Model.Computer_Quad)
qqPlot(Model.Computer_Quad)

profit_Pred<-predict(Model.Computer_Quad)
View(profit_Pred)

finplot <- Computer_data[-c(1441,1701),]
View(finplot)

plot1 <- cbind(finplot$price, profit_Pred)
pairs(plot1)

