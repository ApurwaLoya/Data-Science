library(gmodels)
library(tree)
library(caret)
library(C50)
library(party)
Fraud_Check<-read.csv("E:\\Data Science\\Assignments\\Fraud_check.csv")
View(Fraud_Check)
colnames(Fraud_Check)
hist(Fraud_Check$Taxable.Income)

Risky_Good = ifelse(Fraud_Check$Taxable.Income<= 30000, "Risky", "Good")
Fraud_Check_RG=data.frame(Fraud_Check,Risky_Good)
FC_Train<-Fraud_Check_RG[1:420,]
FC_Test<-Fraud_Check_RG[421:600,]

Tree=ctree(Risky_Good~Undergrad + Marital.Status + City.Population 
                 + Work.Experience + Urban, data=Fraud_Check_RG)
summary(Tree)
plot(Tree)


Train_Tree=ctree(Risky_Good~Undergrad + Marital.Status + City.Population 
                 + Work.Experience + Urban, data=FC_Train)
summary(Train_Tree)
plot(Train_Tree)

pred_Tree<-as.data.frame(predict(Train_Tree, newdata=FC_Test))
pred_Tree_df<-predict(Train_Tree, newdata=FC_Test)

a<-table(FC_Test$Risky_Good,pred_Tree_df)

sum(diag(a))/sum(a)
plot(Train_Tree)

CrossTable(FC_Test$Risky_Good,pred_Tree_df)
