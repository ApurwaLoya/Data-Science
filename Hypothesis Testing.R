
#1. Cutlets
cutlets<-read.csv("E:\\Data Science\\Assignments\\cutlets.csv")
View(cutlets)
attach(cutlets)
library(gmodels)
plot(Unit.A)
plot(Unit.B)
boxplot(cutlets)

#H0: mean of unit.A= mean of Unit.B
#Ha: mean of unit.A != mean of Unit.B

#Two sample t test

t.test(Unit.A,Unit.B,alternative="greater")

#P value > 0.05 hence accept H0


#2.LabTAT

# Ho -> Average TAT for all 4 laboratory is same
# Ha -> Average TAT for atleasr 1 laboratory is different

LabTAT<-read.csv("E:\\Data Science\\Assignments\\LabTAT.csv")
View(LabTAT)
Stacked_Data<-stack(LabTAT)
View(Stacked_Data)
attach(Stacked_Data)
library(car)

leveneTest(values~ ind, data = Stacked_Data)
Anova_results <- aov(values~ind,data = Stacked_Data)
summary(Anova_results)

# p-value < 0.05 reject null hypothesis



#3.Buyer Ratio

#Ho:All proportions are equal
#Ha:Not all Proportions are equal

BR<-read.csv("E:\\Data Science\\Assignments\\BuyerRatio.csv")
View(BR)
boxplot(BR)
st<-stack(BR)
chisq.test(table(ind,values))

# p-value  > 0.05  => Accept null hypothesis
# => All regions have equal proportions 


#4.Customer order form

#Ho:All proportions are equal
#Ha:Not all Proportions are equal

CO<-read.csv("E:\\Data Science\\Assignments\\Costomer_OrderFormV1.csv")
View(CO)
attach(CO)
chisq.test(table(Contry,Val))

# p-value >0.05 accept Null hypothesis ; All proportions are equal


#5. Faltoons

Faltoons<-read.csv("E:\\Data Science\\Assignments\\Faltoons.csv")
View(Faltoons) 
attach(Faltoons)
table1 <- table(Weekdays,Weekend)
table1
prop.test(x=c(167,66),n=c(287,113),conf.level = 0.95,correct = FALSE,alternative = "two.sided")

# p-value >0.05 accept Null hypothesis ; walking is in proportion


