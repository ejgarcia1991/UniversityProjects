#Ejercicio 1
#Pide al usuario que introduzca un string s y un número n y que
#muestre en pantalla n veces seguidas el string s(sin espacios entre
#palabra y palabra).
s <- scan("",what=character())
n <- scan("",what=integer())
cat(rep(s,length.out = n))


#Ejercicio 2
#Crea tres ficheros llamados dos.txt, tres.txt y cinco.txt 
#que contenga la tabla del 2, la del 3 y la del 5 respectivamente
#(los primeros 10 valores de cada tabla, un número en 
#cada línea de cada fichero).
write.table(((1:10)*2), "dos.txt",row.names=FALSE, col.names=FALSE)
write.table(((1:10)*3), "tres.txt",row.names=FALSE, col.names=FALSE)
write.table(((1:10)*5), "cinco.txt",row.names=FALSE, col.names=FALSE)

#Ejercicio 3
#Carga los tres ficheros creados en el punto anterior y 
#construye una matriz que, en cada columna, tenga
#el contenido de cada fichero.
dos <- read.table("dos.txt")
tres <- read.table("tres.txt")
cinco <- read.table("cinco.txt")
matrix <- cbind(dos,tres,cinco)


#Ejercicio 4
#Escribe las cinco primera filas de matriz del ejercicio
#anterior en un fichero nuevo llamado prime.txt y
#las cinco últimas en otro fichero llamado fin.txt.
#Ambos ficheros deben tener los datos separados por comas.
write.table(matrix[1:5,], "prime.txt", row.names=FALSE, col.names=FALSE,sep=",")
write.table(matrix[6:10,], "fin.txt", row.names=FALSE, col.names=FALSE,sep=",")


#Ejercicio 5 
#Dados dos números introducidos por el usuario f y c, 
#crea un cuadrado de f filas y c columnas con el caracter"x".
f <- scan("")
c <- scan("")
matrix(rep(rep("x",f),c),nrow=f)
