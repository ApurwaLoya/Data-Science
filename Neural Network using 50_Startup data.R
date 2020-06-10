library(neuralnet)
library(nnet)
library(NeuralNetTools)
library(plyr)

startups<-read.csv("E:\\Data Science\\Assignments\\50_Startups.csv")
View(startups)
class(startups)
startups$State <- as.numeric(revalue(startups$State,
                                     c("New York"="0", "California"="1",
                                       "Florida"="2")))
str(startups)

startups <- as.data.frame(startups)
attach(startups)

plot(R.D.Spend,Profit)
plot(Administration,Profit)
plot(Marketing.Spend,Profit)
plot(State,Profit)

windows(); pairs(startups)

cor(startups)
summary(startups)

normalize <- function(x) { return((x - min(x)) / (max(x) - min(x)))}
startup_norm<-as.data.frame(lapply(startups,normalize))
summary(startup_norm)
startup_norm<-startup_norm[,-4]

set.seed(123)
ind<-sample(2, nrow(startup_norm), replace = TRUE, prob = c(0.7,0.3))
Startups_train<-startup_norm[ind==1,]
startups_test<-startup_norm[ind==2,]

startups_model<-neuralnet(Profit~R.D.Spend+Administration
                            +Marketing.Spend,data = Startups_train)
str(startups_model)
windows();plot(startups_model)

model_results<-compute(startups_model, startups_test[1:3])
predicted_profit<- model_results$net.result
cor(predicted_profit, startups_test$Profit)

#

startups_model2<-neuralnet(Profit~R.D.Spend+Administration
                          +Marketing.Spend,data = Startups_train, hidden =c(3,2))

windows();plot(startups_model2)

model_results2 <- compute(startups_model2, startups_test[1:3])
predicted_Profit2 <- model_results2$net.result
cor(predicted_Profit2, startups_test$Profit)

