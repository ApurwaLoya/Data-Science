library(datasets)
data("airquality")
airquality<-datasets::airquality
View(airquality)
head(airquality)
tail(airquality)
airquality[,c(1,2)]
airquality$Ozone
summary(airquality$Temp)
summary(airquality)
airquality[1,2]
airquality[1,]
airquality[1:6,]
airquality[1:7,2:3]

par(mfrow=c(2,3),mar=c(2,5,2,1),las=0,bty="o")

plot(airquality$Ozone)
plot(airquality$Ozone,airquality$Temp)
plot(airquality$Ozone, type="b", xlab='ozone Concentration', ylab='No of Insurances' , main='Ozone levels in Pune city', col='pink')
barplot(airquality$Ozone, horz =F)
hist(airquality$Solar.R)
boxplot(airquality[,1:4],main="Multiple")
