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
#Pruebo con k-medoides optimo numero de grupos
pamk.result=pamk(thyroid2)
pamk.result
pamk.result$nc

table(as.vector(pamk.result$pamobject$clustering),thyroid$Class)
plot(pamk.result$pamobject)
#(Ver descripcion de la funcion)
group=pamk.result$pamobject$clustering
cluster.stats(dist(thyroid2),group)
