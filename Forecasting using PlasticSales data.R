PlasticSales<-read.csv('E:\\Data Science\\Assignments\\PlasticSales.csv')

View(PlasticSales) 

plot(PlasticSales$Sales,type="l")

# creating dummy variables 

X<- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )# Creating dummies for 12 months
colnames(X)<-month.abb # Assigning month names 
View(X)
Plasticdata<-cbind(PlasticSales,X)
View(Plasticdata)

Plasticdata["t"]<- 1:60
View(Plasticdata)

Plasticdata["log_Sales"]<-log(Plasticdata["Sales"])
Plasticdata["t_square"]<-Plasticdata["t"]*Plasticdata["t"]

##Data Partition
train<-Plasticdata[1:48,]
test<-Plasticdata[49:60,]

library(Metrics)

########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train)

linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
rmse_linear<-rmse(test$Sales,linear_pred$fit)
rmse_linear

######################### Exponential #################################


expo_model<-lm(log_Sales~t,data=train)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-rmse(test$Sales,exp(expo_pred$fit))
rmse_expo

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-rmse(test$Sales,Quad_pred$fit)
rmse_Quad 

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad


######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame('Model'=c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),'RMSE'=c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))

colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Use entire data : Multiplicative Seasonality Linear trend has least RMSE value
new_model <-lm(log_Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = Plasticdata)

new_model_pred<-data.frame(predict(new_model,newdata=Plasticdata,interval='predict'))

new_model_final <- exp(new_model$fitted.values)

View(new_model_final)

Month <- as.data.frame(Plasticdata$Month)

Final <- as.data.frame(cbind(Month,Plasticdata$Sales,new_model_final))
colnames(Final) <-c("Month","Sales","New_Pred_Value")
Final <- as.data.frame(Final)
View(Final)
