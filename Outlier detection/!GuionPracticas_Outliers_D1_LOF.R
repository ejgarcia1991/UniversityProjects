# M?ster -> Detecci?n de anomal?as
# Juan Carlos Cubero. Universidad de Granada

###########################################################################
# MULTIVARIATE OUTLIERS -> LOF 
###########################################################################


###########################################################################
# RESUMEN:

# El objetivo es encontrar outliers multivariantes con t?cnicas no param?tricas
# Por lo tanto, no exigiremos que haya una distribuci?n estad?stica sobre los datos.
# En este script veremos el m?todo LOF (Local Outlier Factor) y 
# en el siguiente fichero, veremos distintos m?todos basados en Clustering


#####################################################################
# Lectura de valores y Preprocesamiento
#####################################################################


# Tanto LOF como clustering usan distancias entre registros, por lo que habr?
# que trabajar sobre los datos previamente normalizados

# Construimos las siguientes variables:

# mis.datos.numericos -> Contendr? las columnas num?ricas de mtcars, es decir, mtcars[,-c(8:11)] 
# mis.datos.numericos.normalizados-> Contendr? los datos normalizados
# Asignamos como nombres de filas de mis.datos.numericos.normalizados 
# los mismos nombres de filas que mis.datos.numericos
# Para ello, use rownames tanto a la izquierda como a la derecha de la asignaci?n


mis.datos.originales = mtcars[,-c(8:11)] 
mis.datos.numericos  = mis.datos.originales[,sapply(mis.datos.originales, is.numeric)]
mis.datos.numericos.normalizados    = scale(mis.datos.numericos)
rownames(mis.datos.numericos.normalizados) = rownames(mis.datos.numericos)



###########################################################################
###########################################################################
# DISTANCE BASED OUTLIERS (LOF)
###########################################################################
###########################################################################

# Transparencia 118

numero.de.vecinos.lof = 5

# Establecemos el n?mero de vecinos a considerar numero.de.vecinos.lof = 5 y 
# llamamos a la funci?n lofactor pas?ndole como primer par?metro el conjunto 
# de datos normalizados y como par?metro k el valor de numero.de.vecinos.lof
# Esta funci?n devuelve un vector con los scores de LOF de todos los registros
# Lo llamamos lof.scores
# [1] 1.0036218 1.0244637 1.0198058 1.0394019 ......

# Vamos a ver c?mo determinar el n?mero de valores que pueden ser coniderados outliers

# Ordenamos de mayor a menor los lof.scores y obtenemos as? los ?ndices de los registros 

sort(lof.scores,decreasing=TRUE)
plot(sort(lof.scores,decreasing=TRUE))
# indices.segun.lof.score.ordenados
# [1]  6 16 15 31 24 29 17  7  9 ......
# Obtenemos los valores de lof.scores de estos registros:
# lof.scores.ordenados
# [1] 1.7969692 1.7463431 1.7232284 1.6900645 1.5460695 1.4919591  ......
# Realizamos un plot de lof.scores.ordenados y vemos que hay cuatro valores destacadamente
# mayores que el resto. Por lo tanto, establecemos la variable 
# numero.de.outliers = 4
# En general, en un primer an?lisis, tendremos que estableces numero.de.outliers 
# a un valor que creamos conveniente seg?n el plot anterior.

numero.de.outliers=4
mis.datos.numericos.normalizados
indices.de.lof.top.outliers<-c(6,16,15,31)

# Seleccionamos los 4 (numero.de.outliers) primeros y los almacenamos en la variable
# indices.de.lof.top.outliers
# [1]  6 16 15 31

row(mis.datos.numericos.normalizados[,]) %in% indices.de.lof.top.outliers
row(mis.datos.numericos.normalizados) == indices.de.lof.top.outliers
mis.datos.numericos.normalizados[row(mis.datos.numericos.normalizados)%in%indices.de.lof.top.outliers]

is.lof.outlier<-row(mis.datos.numericos)[,1] %in% indices.de.lof.top.outliers


# Construimos un vector is.lof.outlier de TRUE/FALSE que nos dice si cada registro de los datos
# originales es o no un outlier. Para ello, debemos usar la funci?n row sobre el dataset
# y el operador %in% sobre indices.de.lof.top.outliers
# is.lof.outlier
# [1] FALSE FALSE FALSE FALSE FALSE
# Nota: Es posible que en data frames con nombres de filas, deba usar la funci?n rownames en vez de row

# Mostramos un Biplot de los outliers llamando a la funci?n MiBiPlot_Multivariate_Outliers
# MiBiPlot_Multivariate_Outliers = function (datos, vectorTFoutliers, titulo)

# Se puede observar que los 4 outliers caen en la zona exterior de la nube de puntos, 
# por lo que tienen un valor muy alto en alguna de las variables, es decir, son
# outliers uni-dimensionales. En el apartado de ampliaci?n se analiza esa situaci?n
# y se intentar? encontrar outliers multivariantes "puros", es decir, que tengan
# alguna combinaci?n de variables anormal. 






# COMPLETAR  

MiBiPlot_Multivariate_Outliers(mis.datos.numericos.normalizados,is.lof.outlier
,"d")





###########################################################################
# An?lisis de los outliers


# En este apartado queremos ver cu?les son los outliers multivariantes "puros"
# es decir, aquellos outliers que lo son porque tienen una COMBINACI?N
# anormal de valores en varias variables y no porque tengan un valor
# anormal en alguna variable. 
# Vamos a realizar lo siguiente:

# 1. Vamos a analizar cu?les son los outliers porque tienen un valor anormal en alguna variable
#     a) En primer lugar, vamos a obtener una  aproximaci?n gr?fica
#     b) En segundo lugar,  procederemos a cuantificarlo con el m?todo IQR 
# 2. Analizamos los multivariantes "puros" para lo que quitaremos los outliers IQR (1-variantes)


# ----------------------------------------------------------------------
# 1.a) Analizamos los outliers de forma gr?fica.

# Construimos una tabla num?rica 
# data.frame.solo.outliers 
# que muestre los valores normalizados de los outliers en todas las columnas 
# Para ello, usamos mis.datos.numericos.normalizados y is.lof.outlier.
# Nos debe salir:

#                       mpg        cyl        disp         hp       drat        wt        qsec
# Valiant             -0.3302874 -0.1049878 -0.04616698 -0.6080186 -1.5646078 0.2480946  1.32698675
# Cadillac Fleetwood  -1.6078826  1.0148821  1.94675381  0.8504968 -1.2466598 2.0775048  0.07344945
# Lincoln Continental -1.6078826  1.0148821  1.84993175  0.9963483 -1.1157401 2.2553357 -0.01608893
# Maserati Bora       -0.8446439  1.0148821  0.56703942  2.7465668 -0.1057878 0.3605164 -1.81804880

# Mostramos los boxplots de forma conjunta con las etiquetas de los outliers
# Para ello llamamos a la funci?n MiBoxPlot_juntos pasando como par?metro is.lof.outlier
# MiBoxPlot_juntos  = function (datos, vector_TF_datos_a_incluir)  

# Podemos apreciar que el Cadillac, el Lincoln y el Maserati son outliers 1-variantes
# Adem?s, el Valiant est? en los extremos de dos variables (drat y qsec)

# Otra forma alternativa de analizar gr?ficamente los outliers es con un biplot (transparencia 78)
# El BiPlot nos muestra tambi?n los valores extremos en alguna de las variables
# y adem?s, tambi?n nos muestra las correlaciones entre variables
# El precio a pagar es que es una representaci?n aproximada, 
# Los puntos mostrados son resultados de proyecciones de n dimensiones a 2, por lo que 
# s?lo es una representaci?n aproximada (mejor cuanto mayor sea la suma de los  
# porcentajes que aparecen como componentes principales PC1 y PC2)
# Llamamos a la funci?n MiBiPlot_Multivariate_Outliers
# MiBiPlot_Multivariate_Outliers = function (datos, vectorTFoutliers, titulo)
# con mis.datos.numericos y is.lof.outlier

# Podemos apreciar lo mismo que vimos con los BoxPlots.






# COMPLETAR  







# ----------------------------------------------------------------------
# 1.b) Aplicamos el m?todo IQR (1-variante)

# En segunda lugar, una vez visto los gr?ficos, vamos a aplicar el m?todo IQR
# sobre cada variable por separado. Construimos la variable:

# vector.claves.outliers.IQR.en.alguna.columna: Contiene los ?ndices de los que son outliers en ALGUNA columna
#   Hay que llamar a la funci?n vector_claves_outliers_IQR_en_alguna_columna

# Debe salir lo siguiente:
# vector.claves.outliers.IQR.en.alguna.columna
# [1] 20 31 15 16 17  9




# COMPLETAR  






# ----------------------------------------------------------------------
# 2. Analizamos los multivariantes "puros" para lo que quitamos los outliers IQR (1-variantes)

# Construimos la variable
# indices.de.outliers.multivariantes.LOF.pero.no.1variantes: Contiene los outliers LOF que NO son outliers IQR
# Para ello, usamos setdiff sobre indices.de.lof.top.outliers y vector.claves.outliers.IQR.en.alguna.columna
# y vemos que el resultado es 

# indices.de.outliers.multivariantes.LOF.pero.no.1variantes
# [1]  6

# As? pues, en principio, s?lo hay un outlier multivariante "puro" (el 6 -> "Valiant")
# Sin embargo, tal y como muestra el biplot, "Valiant" parece que tiene 
# un valor de qsec muy alto. Efectivamente, si miramos los valores 
# normalizados de este(estos) registro(s), obtenemos:

# valores.normalizados.de.los.outliers.LOF.pero.no.1variantes 
#   mpg         cyl        disp          hp        drat          wt        qsec 
# -0.33028740 -0.10498781 -0.04616698 -0.60801861 -1.56460776  0.24809459  1.32698675 

# Podemos observar que, tal y como aparec?a en el Biplot, "Valiant" tiene un valor
# de qsec bastante elevado. Es m?s, el valor de drat (relaci?n eje trasero) es a?n m?s elevado
# En el Biplot no pareciese que el valor de drat fuese tan elevado, 
# pero no debemos olvidar que el Biplot es una simplificaci?n 
# Es de esperar que los coches con un bajo drat (relaci?n eje trasero) tengan menos consumo
# de combustible (mpg) y menos fuerza (hp) lo cual se confirma en el "Valiant"
# As? pues, no encontramos ninguna combinaci?n de variables anormal en "Valiant"
# y todo indica que es un outlier debido a los valores tan bajos de drat y qsec

# Tenemos pues una situaci?n en la que parece que no hay outliers multivariantes "puros"
# Nos planteamos entonces aumentar el n?mero de outliers LOF a detectar.
# Aumentemos el valor de numero.de.outliers a 12, por ejemplo:
# numero.de.outliers = 12
# volvemos a lanzar el mismo proceso y nos saldr?:
# indices.de.outliers.multivariantes.LOF.pero.no.1variantes
# [1]  6 24 29  7  4 30 25

# Lanzamos MiBiPlot_Multivariate_Outliers sobre is.lof.outlier.pero.no.1.variantes
# Podemos apreciar que los registros 30 -> "Ferrari Dino" y 4 -> "Hornet 4 Drive" 
# son los candidatos a ser multivariantes puros porque no parece 
# que tengan ning?n valor demasiado alto  o bajo en ninguna variable
# (est?n "dentro" de la nube de puntos)
# 
# valores.normalizados.de.los.outliers.LOF.pero.no.1variantes
#                      mpg        cyl        disp         hp        drat           wt       qsec
# Hornet 4 Drive    0.21725341 -0.1049878  0.22009369 -0.5350928 -0.96611753 -0.002299538  0.8904872
# Valiant          -0.33028740 -0.1049878 -0.04616698 -0.6080186 -1.56460776  0.248094592  1.3269868
# Duster 360       -0.96078893  1.0148821  1.04308123  1.4339030 -0.72298087  0.360516446 -1.1241264
# Camaro Z28       -1.12671039  1.0148821  0.96239618  1.4339030  0.24956575  0.636460997 -1.3647608
# Pontiac Firebird -0.14777380  1.0148821  1.36582144  0.4129422 -0.96611753  0.641571082 -0.4469924
# Ford Pantera L   -0.71190675  1.0148821  0.97046468  1.7110209  1.16600392 -0.048290296 -1.8740103
# Ferrari Dino     -0.06481307 -0.1049878 -0.69164740  0.4129422  0.04383473 -0.457097039 -1.3143954

# Nos vamos a centrar en el registro 30, "Ferrari Dino"
# Vamos a ver si son dos o m?s las variables que determinan que es un outlier.
# Vamos a construir una matriz con los gr?ficos de dispersi?n obtenidos 
# al cruzar todas las variables
# Y vamos a destacar en rojo el dato correspondiente a Ferrari Dino.
# Para ello, obtenemos el ?ndice de Ferrari Dino usando las funciones which y rownames
# y llamamos a la funci?n MiPlot_Univariate_Outliers 
# MiPlot_Univariate_Outliers = function (datos, indices_de_Outliers, titulo)
# El par?metro indices_de_Outliers ?nicamente contendr? el ?ndice del Ferrari Dino.
# Puede apreciarse que NO hay una combinaci?n clara de 2 variables que hagan 
# del Ferrari un outlier.

# Por lo tanto, es posible que intervengan m?s de dos variables.
# Efectivamente, si observamos la tabla 
# valores.normalizados.de.los.outliers.LOF.pero.no.1variantes
# o mirando directamente en el Biplot, 
# vemos que Ferrari Dino es un dato con valores interesantes porque tiene:
# - S?lo algo m?s de potencia que los otros coches (hp = 0.4129)
#   El Maserati tiene mucha m?s potencia (2.7)
# - Una aceleraci?n muy notable (qsec = -1.3143) 
#   El Maserati tiene algo m?s, pero tampoco mucho (-1.8)
# - Menos menos cent?metros c?bicos (disp = -0.6916) 
#   El Maserati tiene m?s que la media (0.567)
# Posiblemente lo han podido conseguir gracias a un peso bastante bajo (wt = -0.4570) 
# algo inusual en coches de esas caracter?sticas (El Maserati tiene 0.3)
# En definitiva, hemos encontrado un valor multivariante "puro" en el sentido 
# de que tiene un valor de lof.score bastante alto (si bien no es el mayor de todos) 
# y es debido a una combinaci?n anormal de variables
# (y no a tener un valor muy alto en alguna de las variables).




# COMPLETAR  


