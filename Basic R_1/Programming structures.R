#Ejercicio 1
#Crea una función creciente que indique si los elementos 
#de un vector dado son estrictamente crecientes.
#No se permite ordenar el vector.
creciente <- function(vs){
  i<- 2
  result<-TRUE
  while(i<=length(vs)){
    if(vs[i]<vs[i-1]){
      result<-FALSE
      break()
    }
    i <- i+1
  }
  result
}
vectorprueba<-c(1,5,6,2)
creciente(vectorprueba)

#Ejercicio 2
#Crea una función montecarlo que calcule la estimación 
#de la siguiente integral:
#integral 0 a 1 de x al cuadrado dx
#El algoritmo en pseudo-código para el método de 
#Monte Carlo es el siguiente:
#1. hits=0
#2. for i from 1 to N
#3. Generate two random numbers r1 and r2 between 0 and 1
#4. If r2<r1^2 then hits=hits+1
#5. end for
#6. return hits/n

montecarlo <- function(n){
  hits<-0
  for(i in i:n){
    r1<-runif(1,0,1)
    r2<-runif(1,0,1)
    if(r2<r1^2){
      hits<-hits+1
    }
  }
  hits/n
}
montecarlo(5000)



#Ejercicio 3
#Crea una lista de 5 vectores numéricos y ordena 
#todos los vectores de la lista.
v1 <- sample(1:10,10)
v2 <- sample(11:20,10)
v3 <- sample(21:30,10)
v4 <- sample(31:40,10)
v5 <- sample(41:50,10)
l <- list(v1,v2,v3,v4,v5)
for(i in 1:length(l)){
  l[[i]]<-l[[i]][order(l[[i]])]
}
l

#Ejercicio 4
#Calcula el valor mínimo de cada columna de una matriz, 
#pero toma los valores impares como numeros
#negativos y los pares como positivos.
m<-matrix(1:20,4,5)
temp<-m
temp[temp%%2==1]<-temp[temp%%2==1]*-1
apply(temp,2,min)

#Ejercicio 5
#Dada una matriz devuelva una lista con los valores 
#mayores a 7 de cada fila.
mayoresque7enfila <-function(x){
  l<-(x[x>7])
  l
}
m<-matrix(1:20,4,5)
l<-list()
for(i in 1:nrow(m)){
  l[[i]]<-mayoresque7enfila(m[i,])
}
l
