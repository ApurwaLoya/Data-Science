library(datasets)
data("quakes")
e_quakes<-datasets::quakes
View(e_quakes)
head(e_quakes)
tail(e_quakes)
e_quakes[1:5,3:5]
summary(e_quakes$depth)
summary(e_quakes$mag)
summary(e_quakes$stations)


par(mfrow=c(2,3),mar=c(2,5,2,1),las=0,bty="o")
plot(e_quakes$depth, xlab='quake depth', ylab='values', main='Earth quake depth', col='red')
barplot(e_quakes$mag, main='earth quake magnitude', xlab = 'levels', col='blue')
hist(e_quakes$depth)
hist(e_quakes$mag)
hist(e_quakes$stations)
boxplot(e_quakes[,3:5],main='multiple')

