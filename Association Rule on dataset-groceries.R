library(arules)

groceries<-read.transactions("E:\\Data Science\\Assignments\\groceries.csv", sep=",")

summary(groceries)
inspect(groceries[1:5])
itemFrequency(groceries[, 1:3])
itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, topN = 20)
image(groceries[1:5])
image(sample(groceries, 100))

apriori(groceries)
groceryrules <- apriori(groceries, parameter = list(support =
                                                      0.006, confidence = 0.25, minlen = 2))
groceryrules

summary(groceryrules)

inspect(groceryrules[1:3])

#Improving model performance

inspect(sort(groceryrules, by = "lift")[1:5])

berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

write(groceryrules, file = "groceryrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)
