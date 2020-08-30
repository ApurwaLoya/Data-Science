
library(arules)
Books<-read.csv("E:\\Data Science\\Assignments\\book.csv")
View(Books)

class(Books)

colnames(Books)

Books$ChildBks <- factor(Books$ChildBks,levels = c("1","0"),labels = c("ChildBks",""))
Books$YouthBks <- factor(Books$YouthBks,levels = c("1","0"),labels = c("YouthBks",""))
Books$CookBks <- factor(Books$CookBks,levels = c("1","0"),labels = c("CookBks",""))
Books$DoItYBks <- factor(Books$DoItYBks,levels = c("1","0"),labels = c("DoItYBks",""))
Books$RefBks <- factor(Books$RefBks,levels = c("1","0"),labels = c("RefBks",""))
Books$ArtBks <- factor(Books$ArtBks,levels = c("1","0"),labels = c("ArtBks",""))
Books$GeogBks <- factor(Books$GeogBks,levels = c("1","0"),labels = c("GeogBks",""))
Books$ItalCook <- factor(Books$ItalCook,levels = c("1","0"),labels = c("ItalCook",""))
Books$ItalAtlas <- factor(Books$ItalAtlas,levels = c("1","0"),labels = c("ItalAtlas",""))
Books$ItalArt <- factor(Books$ItalArt,levels = c("1","0"),labels = c("ItalArt",""))
Books$Florence <- factor(Books$Florence,levels = c("1","0"),labels = c("Florence",""))
library(car)
library(carData)
library(mvinfluence)
Book1 <- as(Books,"transactions")
itemFrequencyPlot(Book1,topN=25)

rules <- apriori(Book1, parameter = list(supp = 0.005, confidence = 0.50, minlen = 2, maxlen = 4)) 

inspect(head(sort(rules), n = 10))

plot(head(sort(rules, by = "lift"), n = 10), method = "graph", control = list(cex = 1.0))

plot(rules)

library(arulesViz)

plot(head(sort(rules), n = 10), method = "grouped", control = list(cex = 0.2))
