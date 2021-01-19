#Ejercicio 1
#Mostrar graficamente la informacion correspondiente a summary(iris[1:4]).
#Pista: uso de boxplot
boxplot(iris[1:4])


#Ejercicio 2
#Rellenar una matriz nrow = 200, ncol = 4, con numero aleatorios cada
#columna, supongamos que son las tasas de exito en clasificacion
#correspondiente a 4 algoritmos distintos. Pintar una grafica con las curvas de cada,
#identificando cada uno de los algoritmos con su leyenda.
m<- matrix(sample(1:10000,800),200,4)
opar <- par(no.readonly = TRUE)
plot(m[,1],type="l",col="red")
lines(m[,2],col="blue")
lines(m[,3],col="brown")
lines(m[,4])
legend(x=160, y=10000,legend=c("alg1","alg2","alg3","alg4"), lty=c(1,2,3,4), col=c("red", "blue","brown","black"))


#Ejercicio 3
#Ejecuta las siguientes instrucciones: library(MASS); str(quine); xtabs(~ Age,data=quine);
#prop.table(xtabs(~ Age,data=quine)) Haz un grafico compuesto, con
#dos graficas de barras correspondientes a xtabs y prop.table, la frecuencia
#absoluta y frecuencia relativa de las edades.
library(MASS)
str(quine)
d1<-xtabs(~ Age,data=quine)
d2<-prop.table(xtabs(~ Age,data=quine))
opar<-par(no.readonly=TRUE)
par(mfrow = c(1,2))
barplot(xtabs(~ Age,data=quine),main="Frecuencia absoluta")
barplot(prop.table(xtabs(~ Age,data=quine)), main="Frecuencia relativa")
par(opar)

#Ejercicio 4
#Representa la misma informacion anterior mediante graficas tipo pie y
#dotchart con tıtulo. En pie, fija colores y sentido horario.
library(MASS)
str(quine)
d1<-xtabs(~ Age,data=quine)
d2<-prop.table(xtabs(~ Age,data=quine))
opar<-par(no.readonly=TRUE)
par(mfrow = c(2,2))
dotchart(d1,main="Frecuencia absoluta")
dotchart(d2, main="Frecuencia relativa")

pie(d1,clockwise = TRUE,col = c("red","blue","brown","black"),main="Frecuencia absoluta")
pie(d2,clockwise = TRUE,col = c("red","blue","brown","black"), main="Frecuencia relativa")
par(opar)


#Ejercicio 5
#Sea un dataset cars, representar los puntos dist vs speed, esto es, el
#atributo dist en ordenadas. Sea m, m = lm(speed~dist, data=cars) el
#resultado de aplicar un ajuste mediante regresion lineal. El valor resultado
#es una recta en forma pinta la lınea de ajuste del modelo m, en rojo. Pista: abline.
library(CARS)
m<-lm(speed~dist,data=cars)
plot(cars$dist,cars$speed)
abline(m,col="red")
read_