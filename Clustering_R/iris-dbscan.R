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
#normalizo los datos para DBScan
for (j in 1:5) {
  x=thyroid2[[j]];
  v=(x-mean(x))/sqrt(var(x));
  thyroid2[[j]]=v
}
#Aplico DBscan
ds=dbscan(thyroid2,eps=1.5,MinPts=5)
#Tabla de comparación
table(ds$cluster,thyroid$Class)
#Gráficas
plot(ds,thyroid2)
plot(ds,thyroid2[c(4,5)])
#Silueta para determinar cohesión y separación
x=table(ds$cluster,thyroid$Class)
nc=length(x[,1])
shi= silhouette(ds$cluster,dist(thyroid2))
plot(shi,col=1:nc)



