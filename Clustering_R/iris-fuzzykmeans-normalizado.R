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
#Trabajo con clustering difuso
fuzzy.result=fanny(thyroid2,3,memb.exp=1.4)
str(fuzzy.result)
fuzzy.result$membership
table(fuzzy.result$clustering,thyroid$Class)
plot(fuzzy.result)
#(datos adicionales de la calidad del agrupamiento realizado)
cluster.stats(dist(thyroid),fuzzy.result$clustering,alt.clustering=as.integer(thyroid$Class))
