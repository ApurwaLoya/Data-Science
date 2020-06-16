Wine<- read.csv("E:\\Data Science\\Assignments\\wine.csv")
dim(Wine)
str(Wine)
table(Wine$Type)

#
normalize_dummy <- function(x){
  col <- ncol(x)
  row <- nrow(x)
  y <- 1:nrow(x)
  for (i in 1:col){
    if(class(x[,i])=="numeric" | class(x[,i])=="integer")
    {
      minx <- min(x[,i])
      maxx <- max(x[,i])
      for(j in 1:row)
      {
        x[j,i] <- ifelse((x[j,i] - minx) != 0,yes =((x[j,i] - minx) / (maxx - minx)),no = 0)
      }
    }
    
  }
  f <- c()
  for(i in 1:ncol(x)){
    if(class(x[,i])=="factor"){
      dummies <- data.frame(dummies::dummy(x[,i]))
      y <- data.frame(y,dummies)
      f <- c(f,i)
    }
    else{
      next
    }
  }
  if(is.null(f)){
    output <- x
  }
  else{output <- data.frame(x[,-f],y[,-1])}
  return(output)
}


Wine_norm <- data.frame(Type=Wine$Type,normalize_dummy(Wine[,-1]))
head(Wine_norm)

# Performing PCA for this data
Wine_pca<-princomp(x = Wine_norm[,-1], cor = TRUE, scores = TRUE, covmat = NULL)
summary(Wine_pca)
Wine_pca_2 <- Wine_pca$scores[,1:3]


# Perform Cluster analysis in original data

all_hclust <- function(dist,k=3,method="all"){
  method=c("single", "complete", "average", "mcquitty", "ward.D", "ward.D2", "centroid","median")
  row <- 0
  clust_df <<- as.data.frame(method,matrix(0,nrow = length(method),ncol = k))
  all_clus <- list()
  for(i in method){
    row <- row+1
    hcl <- hclust(d = dist,method = i)
    clusters <- cutree(tree = hcl,k = k)
    all_clus[[i]] <- clusters
    clust_df[row,2:(k+1)] <- as.integer(tabulate(clusters))
  }
  
  all_clus[["table"]] <- clust_df
  return(all_clus)
}

dist_O <-dist(Wine_norm,method = "euclidian")
clustlist_O <- all_hclust(dist = dist_O,k = 3)
clustlist_O$table 

mean(clustlist_O$single==Wine_norm$Type)
mean(clustlist_O$average==Wine_norm$Type)
mean(clustlist_O$mcquitty==Wine_norm$Type)
mean(clustlist_O$ward.D==Wine_norm$Type)
mean(clustlist_O$ward.D2==Wine_norm$Type)

# Performing Clustering on the PCA Data with 3 principal component
dist_2 <-dist(Wine_pca_2,method = "euclidian")
clustlist_2 <- all_hclust(dist = dist_2,k = 3)
clustlist_2$table
mean(clustlist_2$ward.D2==Wine_norm$Type)
table(clustlist_2$ward.D,Wine_norm$Type)
mean(clustlist_2$ward.D==Wine_norm$Type) 
table(clustlist_2$ward.D,Wine_norm$Type)

# Perform Kmeans in Original Data
Twss<-c();k <- 1:7
for(i in 1:7){
  set.seed(133) 
  km_a <- kmeans(x = Wine_norm[,-1],i)
  Twss[i]<-km_a$tot.withinss
}
Twss
plot(y=Twss,x = k,"b",
     col=c(rep(1,2),3,rep(1,4)),cex = c(rep(1,2),2,rep(1,4)),lwd=3,main = 'Scree Plot',
     col.main="blue",col.axis="blue") 

set.seed(133)
km_o <- kmeans(x = Wine_norm[,-1],3)
table(km_o$cluster)
table(Wine_norm$Type)

table(KmeansClusterGroup = km_o$cluster,HierarchicalGroup=Wine_norm$Type)

clusto1 <- ifelse(km_o$cluster == 1,3,ifelse(km_o$cluster==2,1,2))

mean(clusto1==Wine_norm$Type)
table(Kmeans_Groups = clusto1,Hierarchical_groups=Wine_norm$Type)

# Perform Kmeans in PCA Data
Twss<-c();k <- 1:7
for(i in 1:7){
  set.seed(133) 
  km_a <- kmeans(x = Wine_pca_2,i)
  Twss[i]<-km_a$tot.withinss
}
Twss
plot(y=Twss,x = k,"b",
     col=c(rep(1,2),3,rep(1,4)),cex = c(rep(1,2),2,rep(1,4)),lwd=3,main = 'Scree Plot',
     col.main="blue",col.axis="blue")

set.seed(133);km_PCA <- kmeans(x = Wine_pca_2,3)
table(km_PCA$cluster)
table(Wine_norm$Type)
table(KmeansPCA=km_PCA$cluster,HierarchicalOriginal=Wine_norm$Type)
table(KmeansPCA=km_PCA$cluster,KMeansOriginal=Wine_norm$Type)


clust2 <- ifelse(km_PCA$cluster == 1,3,ifelse(km_PCA$cluster==2,1,2))

mean(clust2==Wine_norm$Type)

mean(clust2==clustlist_2$ward.D) 
table(KmeansPCA=clust2,HierarchicalPCA=clustlist_2$ward.D)

mean(clust2==clusto1)
table(KmeansPCA=clust2,KmeansOriginal=clusto1)

#comparing between the two model PCA and Original
mean(clust2==clusto1) 

