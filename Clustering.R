#title: "Machine Learning (Clustering)"
#author: I Gusti Agung Kartika Shanti
#date: August 29, 2019
---
#Description : To apply k-means clustering and hierarchical clustering
#Data provided on https://github.com/arikunco/machinelearning/blob/master/dataset/online_retail_clean.csv

library(cluster)
library(DataExplorer)

#loading iris database
x <- read.csv("https://raw.githubusercontent.com/arikunco/machinelearning/master/dataset/online_retail_clean.csv")

#liat 6 data pertama
head(x)

#Akan diambil kolom ke 2 sampe 4
x<-x[,2:4]
head(x)


#Berkenalan dulu dengan data (hint: summary(), dim(), plot())
summary(x)

#Determine Number of Clusters using Elbow Method
#Initialize total within sum of squares error: wss
wss <- 0

#For 1 to 15 cluster centers
for (i in 1:15) {
  km.out <- kmeans(x[,1:3], centers = i, nstart = 20)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}


#Plot total within sum of squares vs. number of clusters
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

#Determine Number of Clusters using Silhouette Method
silhouette_score <- function(k){
  km <- kmeans(x, centers = k, nstart=20)
  ss <- silhouette(km$cluster, dist(x))
  mean(ss[, 3])
}

#For 2 to 15 clusters
k <- 2:15
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

#Berdasarkan hasil yang dijalankan menggunakan elbow method, cluster optimalnya adalah 3
#Berdasarkan hasil yang dijalankan menggunakan silhoutte method, cluster optimalnya adalah 3
#Maka banyaknya cluster yang digunakan adalah 3.

#Menggunakan K-Means dengan K=3
km <- kmeans(x, centers = 3, nstart=20)
plot(x, col = km$cluster,
     main = "k-means with 3 clusters")

#Membership
km$cluster

#Berikut adalah data yang masuk kedalam cluster 1
which(km$cluster %in% 1)
paste("Data in cluster: ",as.character(length(which(km$cluster %in% 1))))

#Berikut adalah data yang masuk kedalam cluster 2
which(km$cluster %in% 2)
paste("Data in cluster: ",as.character(length(which(km$cluster %in% 2))))

#Berikut adalah data yang masuk kedalam cluster 3
which(km$cluster %in% 3)
paste("Data in cluster: ",as.character(length(which(km$cluster %in% 3))))

#Data beserta cluster akan disimpan pada data frame bernama df
df<-cbind(x,cluster=km$cluster)
head(df)

#Ini adalah 6 data pertama yang masuk pada cluster 1
head(df[df$cluster==1,])
summary(df[df$cluster==1,])[,1:3]

#Ini adalah 6 data pertama yang masuk pada cluster 2
head(df[df$cluster==2,])
summary(df[df$cluster==2,])[,1:3]

#Ini adalah 6 data pertama yang masuk pada cluster 3
head(df[df$cluster==3,])
summary(df[df$cluster==3,])[,1:3]

#Berikut adalah center dari k-means dengan cluster 3
km$centers


---------------------------------------------------------------------------------------------------------

#Kode untuk Hierarchical Clustering

#Berkenalan dulu dengan data
summary(x)
dim(x)
plot(x)

#Calculates similarity as Euclidean distance between observations
d <- dist(x[,1:3])

#Returns hierarchical clustering model
hclust.out <- hclust(d = d)
summary(hclust.out)


#Draws a dendrogram
layout(1)
plot(hclust.out)
abline(h = 3.5, col = "pink")

#Cut by height h
cutree(hclust.out, h = 6)


#Cut by number of clusters
cutree(hclust.out, k = 3)

#Data yang masuk ke cluster 1
which(cutree(hclust.out, k = 3) %in% 1)
paste("Data in cluster: ",as.character(length(which(cutree(hclust.out, k = 3) %in% 1))))

#Data yang masuk cluster 2
which(cutree(hclust.out, k = 3) %in% 2)
paste("Data in cluster: ",as.character(length(which(cutree(hclust.out, k = 3) %in% 2))))

#Data yang masuk cluster 3
which(cutree(hclust.out, k = 3) %in% 3)
paste("Data in cluster: ",as.character(length(which(cutree(hclust.out, k = 3) %in% 3))))


#Fitting hierarchical clustering models using different methods
hclust.complete <- hclust(d, method = "complete")
hclust.average <- hclust(d, method = "average")
hclust.single <- hclust(d, method = "single")

plot(hclust.complete)

plot(hclust.average)

plot(hclust.single)
