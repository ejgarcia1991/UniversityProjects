#Ejercicio 1
#Crea un vector de strings que contengan 3 datos: tu primer nombre
#y tus dos apellidos. A partir de éste crea un nuevo string
#con la inicial de tu nombre (y un punto) y el apellido 
#completo utilizando las utilidades de R. En mi caso debería
#quedar:R. Romero Zaliz.

nombre <- "Eilder Jorge García"
LetraInicial <- paste(substring(nombre,1,1),".",sep = "")
nombreYApellidos <-unlist(strsplit(nombre," "))
paste(LetraInicial,nombreYApellidos[2],nombreYApellidos[3])

#Ejercicio 2
#Dado un vector de fechas, expresadas como strings 
#(e.g., “2005-11-28”), muestra solamente aquellas
#correspondientes a los meses impares.
vectorfechas <- c("2005-11-28","2005-03-28","2005-02-28","2005-09-28","2005-06-28","2005-12-28")
vectorfechas[as.integer(substring(vectorfechas,6,7))%%2==1]

#Ejercicio 3
#Dado un string con varias palabras (e.g., “Esta es una frase, 
#pero no cualquier frase.”) crea un vector con cada una
#de las plabras del string (["Esta","es","una","frase",
#"pero","no","cualquier","frase"])
stringEjercicio <- "Esta es una frase de prueba para el ejercicio"
vectorResultado <-unlist(strsplit(stringEjercicio," "))
vectorResultado

#Ejercicio 4
#Busca las palabras que usan solamente las vocales
#“a” y “e” en un vector de strings.
grep("a|e",vectorResultado)
vectorResultado[grep("a|e",vectorResultado)]

#Ejercicio 5
#Dados tres vectores dia, mes y anno crea un vector
#con las fechas completas. Si la fecha es inválida,
#ésta se descartará (hint: investiga la función as.Date)

vectorAnno <- c(2018:2019)
vectorMes <- c(01:12)
vectorDia <- c(01:31)
as.Date(paste(vectorAnno,vectorMes,vectorDia,sep="-"))

