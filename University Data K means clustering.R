#Data load
Universities<-read.csv("E:\\Data Science\\Class\\Universities.csv")


# Elbow method
install.packages('factoextra')
library(factoextra)
fviz_nbclust(Universities[,-1], kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

###Cluster algorithm building
km <- kmeans(Universities[,-1],4) 
km$centers
km$cluster

##Animation
install.packages("animation")
library(animation)
windows()
km <- kmeans.ani(Universities[,-1], 4)
