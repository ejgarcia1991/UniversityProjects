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

#Genero distancia numerico
dist1=dist(thyroid2)
#No se posee distancia binaria para este dataset
#Calculo clúster
h=hclust(dist1,method="ward.D2")
h
plot(h,labels=thyroid$Class)
rect.hclust(h,k=3)
grupo=cutree(h,k=3)
#Analsis de bondad, restrinjo a 150 valores para dibujar
plotcluster(thyroid2,grupo)
shi= silhouette(grupo,dist1)
plot(shi,col=1:3)
cluster.stats(dist1,grupo)