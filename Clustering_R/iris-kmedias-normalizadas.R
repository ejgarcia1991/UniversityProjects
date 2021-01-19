#Cargo las librerías que voy a utilizar en el código
library(readr)
library(fpc)
library(cluster)
#Cargo el dataset
thyroid <- read_csv("newthyroid/newthyroid.dat",col_names = FALSE, col_types = cols("X6" = col_factor(levels = c("1","2", "3"))), skip = 10)
names_thyroid<-scan("newthyroid/newthyroid.dat",what=character(),sep=" ",nlines=11)
names_thyroid[seq(4,32,by=5)]
names(thyroid)<-names_thyroid[seq(4,32,by=5)]
thyroid2=thyroid
thyroid2$Class=NULL
#normalizo los datos
for (j in 1:5) {
  x=thyroid2[[j]];
  v=(x-mean(x))/sqrt(var(x));
  thyroid2[[j]]=v
}
#Aplico K-means
kmeans.result=kmeans(thyroid2,3)
kmeans.result
#Tabla de comparación
table(thyroid$Class,kmeans.result$cluster)

#Analisis de bondad
plot(thyroid[2:3], col=kmeans.result$cluster)
points(kmeans.result$centers[2:3],col=1:3,pch=8,cex=2)
x=kmeans.result$cluster
plotcluster(thyroid2,x)
shi= silhouette(kmeans.result$cluster,dist(thyroid2))
plot(shi,col=1:3)
#Calculo de algunas otras medidas de bondad del agrupamiento
group=kmeans.result$cluster
cluster.stats(dist(thyroid2),group,alt.clustering=as.integer(thyroid$Class))