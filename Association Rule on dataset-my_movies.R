mymovies<-read.csv("E:\\Data Science\\Assignments\\my_movies.csv")
View(mymovies)
rules <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.1, confidence = 0.5,minlen=5)))
rules
inspect(head(sort(rules, by = "lift")))  
head(quality(rules))
plot(rules,method = "scatterplot")
plot(rules, method = "grouped")
plot(rules,method = "graph")


rules <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.2, confidence = 0.5,minlen=5)))
rules
inspect(head(sort(rules, by = "lift")))  
head(quality(rules))
plot(rules,method = "scatterplot")
plot(rules, method = "grouped")
plot(rules,method = "graph")


rules <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.1, confidence = 0.1,minlen=5)))
rules
inspect(head(sort(rules, by = "lift")))  
head(quality(rules))
plot(rules,method = "scatterplot")
plot(rules, method = "grouped")
plot(rules,method = "graph")
