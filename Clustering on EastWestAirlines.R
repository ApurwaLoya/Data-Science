
#hierarchical clustering

library(readxl)

AirLine_DF<-read_excel("E:\\Data Science\\Assignments\\EastWestAirlines.xlsx",sheet="data")

AirLine_DF$cc1_miles = ifelse(AirLine_DF$cc1_miles==1,2500,
                              ifelse(AirLine_DF$cc1_miles==2,7500,
                                     ifelse(AirLine_DF$cc1_miles==3,17500,
                                            ifelse(AirLine_DF$cc1_miles==4,32500,
                                                   ifelse(AirLine_DF$cc1_miles==5,50000,0)))))

AirLine_DF$cc2_miles = ifelse(AirLine_DF$cc2_miles==1,2500,
                              ifelse(AirLine_DF$cc2_miles==2,7500,
                                     ifelse(AirLine_DF$cc2_miles==3,17500,
                                            ifelse(AirLine_DF$cc2_miles==4,32500,
                                                   ifelse(AirLine_DF$cc2_miles==5,50000,0)))))

AirLine_DF$cc3_miles = ifelse(AirLine_DF$cc3_miles==1,2500,
                              ifelse(AirLine_DF$cc3_miles==2,7500,
                                     ifelse(AirLine_DF$cc3_miles==3,17500,
                                            ifelse(AirLine_DF$cc3_miles==4,32500,
                                                   ifelse(AirLine_DF$cc3_miles==5,50000,0)))))


data = scale(AirLine_DF)

d <- dist(data[,2:11], method = "euclidean") 

fit <- hclust(d, method="ward.D2")
fit <- as.dendrogram(fit)
library(dendextend)
cd = color_branches(fit,k=3) #Coloured dendrogram branches
plot(cd)

clusters <- cutree(fit, k=3) 
table(clusters)

g1 = aggregate(AirLine_DF[,2:11],list(clusters),median)
data.frame(Cluster=g1[,1],Freq=as.vector(table(clusters)),g1[,-1])


centroid = function(i, dat, clusters) 
{
  ind = (clusters == i)
  colMeans(dat[ind,])
}

sapply(unique(clusters), centroid, AirLine_DF[,2:11], clusters)


#Kmeans clustering

km.3 <- eclust(data[,2:11], "kmeans", k = 3, nstart = 25, graph = TRUE)


fviz_silhouette(km.3)
