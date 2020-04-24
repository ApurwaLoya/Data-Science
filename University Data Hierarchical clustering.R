#Data load
mydata1<-read.csv("E:\\Data Science\\Class\\Universities.csv")

################################
mydata <- scale(mydata1[,2:7])

d <- dist(mydata, method = "euclidean") #Computing the distance matrix
fit <- hclust(d, method="average") # Building the algorith
plot(fit) # display dendogram
clusters <- cutree(fit, k=5) # cut tree into 4 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")
#Attach the cluster numbers to Uni
clusters=data.frame('Uni'=mydata1[,1],'Cluster' =clusters)

fit1 <- hclust(d, method="centroid")
plot(fit1)


fit2 <- hclust(d, method="complete")
plot(fit2)

fit3 <- hclust(d, method="ward.D2")
plot(fit3)
