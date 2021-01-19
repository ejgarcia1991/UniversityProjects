#Ejercicio 1
#Crea una función impares que dado un vector 
#devuelva la cantidad de elementos impares que contiene.
impares <- function(vec){
  sum(vec%%2==1)
  
}
impares(c(1:250))

#Ejercicio 2
#Crea una función cambio que dada una matriz 
#de numeros enteros reemplaze todos los NA por el valor 0.
cambio <- function(mat){
  mat[is.na(mat)] <- 0
  mat
}
matrizprueba <- matrix(c(NA,NA,1:4),c(2:5,NA))
cambio(matrizprueba)

#Ejercicio 3
#Crea una función unir que dados dos vectores 
#devuelva un nuevo vector con los elementos de ambos
#vectores sin repetidos.
unir <- function(vecx, vecy){
  union(vecx,vecy)
}
vectorprueba1 <- c(1:20)
vectorprueba2 <- c(10:30)
unir(vectorprueba1,vectorprueba2)

#Ejercicio 4
#Crea una función vyc que dado un string devuelva 
#una lista de dos componentes que contenga las
#vocales y las consonantes.
vyc <- function(stringpar){
  vocales<-lengths(regmatches(stringpar, gregexpr("[aeiou]",stringpar)))
  consonantes<-lengths(regmatches(stringpar, gregexpr("[^aeiou]",stringpar)))
  list(vocales,consonantes)
}
stringejemplo <- c("Este es un string de ejemplo para el ejercicio")
vyc(stringejemplo)

#Ejercicio 5
#Crea una función partir que dado un vector v y dos 
#valores x e y (siendo y opcional), retorne un
#vector con los valores que aparecen luego del primer x y 
#hasta el primer y. De no indicarse el valor de y
#se devolveran todos los valores que aparecen luego del 
#primer x hasta el final del vector.
partir <- function(v, x, y=NA){
  res<-0
  if(!is.na(y))
    res<-v[x:y]  
  
  else
    res<-v[x:length(v)]
  
  res
}
vectorprueba <- c(1:500)
x<-356
y<-400
partir(vectorprueba,x,y)
partir(vectorprueba,x)