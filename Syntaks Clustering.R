#Required packages
library(cluster)
library(dendextend)
library(factoextra)
library(ggplot2)
library(tidyverse)
library(fpc)
library(fclust)
library(ppclust)

#Load Datasets
data=read.xlsx(file.choose(),sheetName="Sheet1")
#Statistik Deskriptif
summary(data)

#EDA
ggplot(data, aes(X1, X2)) + geom_point()

#Jika perlu standarisasi
data=scale(data)
data=as.data.frame(data)

#EDA
ggplot(data, aes(X1, X2)) + geom_point()

#Preprocessing
#Deteksi Missing value
data[which(data=="NA"),]
which(data=="NA")
is.na(data)
#Mean imputation
for(i in 1:ncol(data)) {
  data[ , i][is.na(data[ , i])] <- mean(data[ , i], na.rm = TRUE)
}
#Median imputation
for(i in 1:ncol(data)) {
  data[ , i][is.na(data[ , i])] <- median(data[ , i], na.rm = TRUE)
}

#Deteksi Outlier
win.graph()
par(mfrow=c(1,3))
for(i in 1:3) {
  boxplot(data[,i], main=names(data)[i],col="red")
}
attach(data)

#Korelasi
pairs.panels(data,gap=0)
cor(data)
corrplot(cor(data),method = "ellipse",type="upper")

#Dimensional Reduction
fit_pca <- princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)
summary(fit_pca)
loadings(fit_pca)
plot(fit_pca,type="lines")
data_baru=fit_pca$scores
pca2d(fit.pca)
pca3d(fit.pca)


#1. K-Means
fviz_nbclust(data,kmeans,method="wss")
fviz_nbclust(data,kmeans,method="silhouette")

set.seed(20)
Cluster <- kmeans(data, 3)
Cluster
Cluster$cluster <- as.factor(Cluster$cluster)
ggplot(data, aes(data[,1],data[,2], color = Cluster$cluster)) + geom_point()
fviz_cluster(Cluster,data=data)

#2. Agnes
fviz_nbclust(data,hcut,hc_metric = "euclidean",hc_func = "agnes", method = "silhouette",hc_method = "single",stand = TRUE)
fviz_nbclust(data,hcut,hc_metric = "euclidean",hc_func = "agnes", method = "silhouette",hc_method = "complete",stand = TRUE)
fviz_nbclust(data,hcut,hc_metric = "euclidean",hc_func = "agnes", method = "silhouette",hc_method = "average",stand = TRUE)
fviz_nbclust(data,hcut,hc_metric = "euclidean",hc_func = "agnes", method = "silhouette",hc_method = "ward",stand = TRUE)

#Analisis berdasar kluster optimal
clust1 = hcut(data, k=3, hc_metric = "euclidean",hc_func = "agnes", method = "silhouette",hc_method = "single",stand = TRUE)
clust2 = hcut(data, k=2, hc_metric = "euclidean",hc_func = "agnes", method = "silhouette",hc_method = "complete",stand = TRUE)
clust3 = hcut(data, k=2, hc_metric = "euclidean",hc_func = "agnes", method = "silhouette",hc_method = "average",stand = TRUE)
clust4 = hcut(data, k=2, hc_metric = "euclidean",hc_func = "agnes", method = "silhouette",hc_method = "ward",stand = TRUE)

#Agglomerative Coefficients
clust1$ac
clust2$ac
clust3$ac
clust4$ac

#dendogram Agglomerative
fviz_dend(clust1, rect = TRUE)
fviz_dend(clust2, rect = TRUE)
fviz_dend(clust3, rect = TRUE)
fviz_dend(clust4, rect = TRUE)

#plot
fviz_cluster(clust1,data=data)
fviz_cluster(clust2,data=data)
fviz_cluster(clust3,data=data)
fviz_cluster(clust4,data=data)

#3. Diana
fviz_nbclust(data,hcut,hc_metric = "euclidean",hc_func = "diana", method = "silhouette",stand = TRUE)
fviz_nbclust(data,hcut,hc_metric = "manhattan",hc_func = "diana", method = "silhouette", stand = TRUE)

#pemilihan cluster optimal
div1 = hcut(data, k=2, hc_metric = "euclidean", stand = TRUE, hc_func = "diana")
div2 = hcut(data, k=2, hc_metric = "manhattan", stand = TRUE, hc_func = "diana")

#Divisive Coefficients
div1$dc
div2$dc

#dendogram divisive
fviz_dend(div1, rect = TRUE)
fviz_dend(div2, rect = TRUE)

#plot
fviz_cluster(div1,data=data)
fviz_cluster(div2,data=data)

#4. K-Medoids
fviz_nbclust(data,pam,method="wss")
fviz_nbclust(data,pam,method="silhouette")
pam1=pamk(data)
pam1
pam1$pamobject$clustering
layout(matrix(c(1,2),1,2))
plot(pam1$pamobject)
layout(matrix(1))
clusplot(data,pam1$pamobject$clustering, 
         main='2D representation of the Cluster',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)
#Pam2
pam2=pam(data,3,stand=F)
pam2
layout(matrix(c(1,2),1,2))
plot(pam2)
layout(matrix(1))
clusplot(data,pam2$clustering, 
         main='2D representation of the Cluster',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#Clara
fviz_nbclust(data,clara,method="wss")
fviz_nbclust(data,clara,method="silhouette")
clara.ku <- clara(data, k=2, samples = 50, pamLike = TRUE)
print(clara.ku)
clara.ku$medoids
plot(clara.ku)
clusplot(data,clara.ku$clustering, 
         main='2D representation of the Cluster',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)


#5. DBSCAN
ds <- dbscan(data, eps=0.42, MinPts=5)
ds
plot(ds,data)
plotcluster(data, ds$cluster)
clusplot(data,ds$cluster, 
         main='2D representation of the Cluster',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)
