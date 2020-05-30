library(gmodels)
library(tree)
library(caret)
library(C50)
library(party)
Company_Data<-read.csv("E:\\Data Science\\Assignments\\Company_Data.csv")
View(Company_Data)
hist(Company_Data$Sales)
summary(Company_Data$Sales)

High = ifelse(Company_Data$Sales<10, "No", "Yes")
CD = data.frame(Company_Data, High)

CD_train <- CD[1:320,]
CD_test <- CD[321:400,]

colnames(Company_Data)

Tree_Train = ctree(High ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc
                + Age + Education + Urban + US, data = CD_train)
summary(Tree_Train)
plot(Tree_Train)

pred<- as.data.frame(predict(Tree_Train, newdata=CD_test))
pred["final"] <- NULL
pred_df <- predict(Tree_Train,newdata=CD_test)

mean(pred_df==CD$High)
CrossTable(CD_test$High,pred_df)
confusionMatrix(CD_test$High,pred_df)
