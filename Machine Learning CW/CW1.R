#Importing the packages
library(ggplot2)
library(readxl)
library(dplyr)
library(cluster)
library(factoextra)
library(NbClust)
library(caret)
library(tidyverse)
#using a new variable to retrieve the data set
mydata<- read_excel("C:/Users/Achintha Rodrigo/Desktop/CW/vehicles.xlsx")
mydata<- mydata[, 2:19]
View(mydata)
boxplot(mydata)
summary(mydata)
# identify the outliers
outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  
  return(x < (Q1 - 1.5 * iqr) | x > (Q3 + 1.5 * iqr))
}

#remove outliers
remove_outliers <- function(mydata, cols = names(mydata)) {
  for (col in cols) {
    mydata<- mydata[!outliers(mydata[[1]]),]
  }
  mydata
}

#allocating the outliers to a new dataset after eliminating them from the original one
newdata <- remove_outliers(mydata[, 2:18], c('Comp','Circ','D.Circ','Rad.Ra','Pr.Axis.Ra','Max.L.Ra','Scat.Ra','Elong','Pr.Axis.Rect','Max.L.Rect','Sc.Var.Maxis','Sc.Var.maxis','Ra.Gyr','Skew.Maxis','Skew.maxis','Kurt.maxis','Kurt.Maxis','Holl.Ra','Class'
))

#the data set is plotted without using outliers
boxplot(newdata)
cleanedNew <- subset(newdata, select = -(Holl.Ra))
vehicleDataScale <- scale(newdata)
View(vehicleDataScale)
#the revised scaled data set without the outliers being plotted
boxplot(vehicleDataScale)
#using the elbow approach to center the clusters
fviz_nbclust(vehicleDataScale, kmeans, method = "wss")+
  labs(subtitle = "Elbow Method")

#the Silhouette method of obtaining cluster centers
fviz_nbclust(vehicleDataScale, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
# Gap statistics
gap_stat <- clusGap(vehicleDataScale, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat) + geom_vline(xintercept = gap_stat$bestK, linetype = "dashed")
best_k_gap_stat <- gap_stat$bestK

#acquiring cluster nodes with NbClust
clusterNo = NbClust(vehicleDataScale, distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")
clusterNo
# k = 2
km.out2 <- kmeans(vehicleDataScale, centers =2, nstart =50)
# results
km.out2
km2.clusters <- km.out2$cluster
#plotting  k = 2 
fviz_cluster(list(data=vehicleDataScale, cluster = km2.clusters))
# TSS 
km.out2$totss
# BSS 
km.out2$betweenss
# WSS 
km.out2$withinss
# ratio  BSS : TSS
ratio2 <- km.out2$betweenss / km.out2$totss
# Cluster Centers
km.out2$centers
# k = 3
km.out3 <- kmeans(vehicleDataScale, centers =3, nstart =50)
# results
km.out3
km3.clusters <- km.out3$cluster
#plotting k = 3 
fviz_cluster(list(data=vehicleDataScale, cluster = km3.clusters))

# TSS 
km.out3$totss
# BSS
km.out3$betweenss
# WSS 
km.out3$withinss
# ratio BSS/TSS
ratio3 <- km.out3$betweenss / km.out3$totss
ratio3
# Cluster Centers
km.out3$centers
kam3 <- kmeans(vehicleDataScale, centers = 3, nstart =50)
kam4 <- kmeans(newdata, centers = 2, nstart =50)

newScaled <- vehicleDataScale
#applying PCA
transformed.pca <- prcomp(newScaled, center = TRUE,scale. = TRUE)
plot(transformed.pca)


#using computers to create a fresh dataset and calculate a cumulative score > 92%
newTransformed.cpa <- as.data.frame(transformed.pca$x[,1:6])
plot(newTransformed.cpa)

km.pca <- kmeans(newTransformed.cpa,centers = 3)
fviz_cluster(list(data = newTransformed.cpa, cluster = km.pca$cluster))
 #TSS 
km.pca$totss
# BSS 
km.pca$betweenss
# BSS/TSS
ratio.pca <- km.pca$betweenss / km.pca$totss
ratio.pca
km.pca$centers


# Calculate Calinski-Harabasz Index
calinski_harabasz_pca <- function(cluster_result, data) {
  k <- length(unique(cluster_result$cluster))
  n <- nrow(data)
  BSS <- km.out3$betweenss
  WSS <- km.out3$withinss
  
  ch_index <- ((n - k) / (k - 1)) * (BSS / WSS)
  return(ch_index)
}

ch_index_pca <- calinski_harabasz_pca(data,newTransformed.cpa)
ch_index_pca
