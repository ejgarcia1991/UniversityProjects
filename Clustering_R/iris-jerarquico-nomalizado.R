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
#calculo cluster jeraquico por el metodo de Ward, distancia euclidea
hc=hclust(dist(thyroid2),method="ward.D2")
hc
#dibujo el dendrograma y corto por tres
plot(hc,labels=thyroid$Class)
rect.hclust(hc,k=3)
#genero la variable de agrupamiento
group=cutree(hc,k=3)
group
table(thyroid$Class,group)
#Medidas de bondad de agrupamiento: coeficiente de silueta
plotcluster(thyroid2,group)
shi= silhouette(group,dist(thyroid2))
plot(shi,col=1:3)
#Calculo de algunas otras medidas de bondad del agrupamiento
cluster.stats(dist(thyroid2),group,alt.clustering=as.integer(thyroid$Class))
