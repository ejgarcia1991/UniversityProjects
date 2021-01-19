# M?ster -> Detecci?n de anomal?as
# Juan Carlos Cubero. Universidad de Granada

###########################################################################
# UNIVARIATE STATISTICAL OUTLIERS -> IQR 
###########################################################################


###########################################################################
# RESUMEN:

# El objetivo es calcular los outliers 1-variantes, es decir, 
# con respecto a una ?nica variable o columna.
# Se va a utilizar el m?todo IQR que, aunque en principio, es s?lo aplicable
# a la distribuci?n normal, suele proporcionar resultados satisfactorios
# siempre que los datos no sigan distribuciones "raras" como por ejemplo 
# con varias modas o "picos"

# Da igual si los datos est?n normalizados o no.


############################################################################


# Cuando necesite lanzar una ventana gr?fica, ejecute X11()

# Vamos a trabajar con los siguientes objetos:

# mydata.numeric: frame de datos
# indice.columna: ?ndice de la columna de datos de mydata.numeric con la que se quiera trabajar
# nombre.mydata:  Nombre del frame para que aparezca en los plots

# En este script usaremos:

mydata.numeric  = mtcars[,-c(8:11)]  # mtcars[1:7]
indice.columna  = 1
nombre.mydata   = "mtcars"

# ------------------------------------------------------------------------

# Ahora creamos los siguientes objetos:

# mydata.numeric.scaled -> Debe contener los valores normalizados de mydata.numeric. Para ello, usad la funci?n scale
# columna -> Contendr? la columna de datos correspondiente a indice.columna. Basta realizar una selecci?n con corchetes de mydata.numeric
# nombre.columna -> Debe contener el nombre de la columna. Para ello, aplicamos la funci?n names sobre mydata.numeric
# columna.scaled -> Debe contener los valores normalizados de la anterior


mydata.numeric.scaled = scale(mydata.numeric)
columna         = mydata.numeric[, indice.columna]
nombre.columna  = names(mydata.numeric)[indice.columna]
columna.scaled  = mydata.numeric.scaled[, indice.columna]



###########################################################################
###########################################################################
# Parte primera. C?mputo de los outliers IQR
###########################################################################
###########################################################################



###########################################################################
# Calcular los outliers seg?n la regla IQR. Directamente sin funciones propias
###########################################################################

# Transparencia 80


# ------------------------------------------------------------------------------------

# Calculamos las siguientes variables:

# cuartil.primero -> primer cuartil, 
# cuartil.tercero -> tercer cuartil
# iqr             -> distancia IQR

# Para ello, usamos las siguientes funciones:
# quantile(columna, x) para obtener los cuartiles
#    x=0.25 para el primer cuartil, 0.5 para la mediana y 0.75 para el tercero
# IQR para obtener la distancia intercuartil 
#    (o bien reste directamente el cuartil tercero y el primero)

# Calculamos las siguientes variables -los extremos que delimitan los outliers-

# extremo.superior.outlier.normal  = cuartil tercero + 1.5 IQR
# extremo.inferior.outlier.normal  = cuartil primero - 1.5 IQR
# extremo.superior.outlier.extremo = cuartil tercero + 3 IQR
# extremo.inferior.outlier.extremo = cuartil primero - 3 IQR

# Construimos sendos vectores: 

# vector.es.outlier.normal 
# vector.es.outlier.extremo

# Son vectores de valores l?gicos TRUE/FALSE que nos dicen 
# si cada registro es o no un outlier con respecto a la columna fijada
# Para ello, basta comparar con el operador > o el operador < la columna con alguno de los valores extremos anteriores

# El resultado debe ser el siguiente:
# [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [18] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE






# COMPLETAR  
columna
MiBoxPlot_IQR_Univariate_Outliers(mydata.numeric.scaled,indice.columna)
vector_es_outlier_IQR(mydata.numeric,indice.columna)


###########################################################################
# ?ndices y valores de los outliers
###########################################################################

# Construimos las siguientes variables:

# claves.outliers.normales     -> Vector con las claves (identificador num?rico de fila) de los valores que son outliers. Para obtenerlo, usad which sobre vector.es.outlier.normal
# data.frame.outliers.normales -> data frame obtenido con la selecci?n del data frame original de las filas que son outliers. Puede usarse o bien vector.es.outlier.normal o bien claves.outliers.normales
#                                 Este dataframe contiene los datos de todas las columnas de aquellas filas que son outliers.                                  
# nombres.outliers.normales    -> vector con los nombres de fila de los outliers. Para obtenerlo, usad row.names sobre el data frame anterior
# valores.outliers.normales    -> vector con los datos de los outliers. Se muestra s?lo el valor de la columna que se fij? al inicio del script 
# Idem con los extremos

# Nos debe salir lo siguiente:

# claves.outliers.normales
# [1] 20

# data.frame.outliers.normales
#                  mpg cyl disp hp drat  wt qsec
# Toyota Corolla 33.9   4 71.1 65 4.22 1.835 19.9

# nombres.outliers.normales
# [1] "Toyota Corolla"

# valores.outliers.normales
# [1] 33.9

#  Outliers extremos no sale ninguno





# COMPLETAR  
vectoroutliers<-vector_es_outlier_IQR(mydata.numeric,indice.columna)
claves.outliers.normales<-which(vectoroutliers==TRUE)
data.frame.outliers.normales<-mydata.numeric[vectoroutliers,]                                  
nombres.outliers.normales<-row.names(data.frame.outliers.normales)
valores.outliers.normales<-data.frame.outliers.normales[,indice.columna]



###########################################################################
# Desviaci?n de los outliers con respecto a la media de la columna
###########################################################################

# Construimos la variable:

# valores.normalizados.outliers.normales -> Contiene los valores normalizados de los outliers. 
# Usad columna.scaled y (o bien vector.es.outlier.normal o bien claves.outliers.normales)

# Nos debe salir:
# valores.normalizados.outliers.normales
# Toyota Corolla   2.291272 




# COMPLETAR  
valores.normalizados.outliers.normales<- columna.scaled[claves.outliers.normales]




###########################################################################
# Plot
###########################################################################

# Mostramos en un plot los valores de los registros (los outliers se muestran en color rojo)
# Para ello, llamamos a la siguiente funci?n:
# MiPlot_Univariate_Outliers (columna de datos, indices -claves num?ricas- de outliers , nombre de columna)
# Lo hacemos con los outliers normales y con los extremos






# COMPLETAR  
MiPlot_Univariate_Outliers(mydata.numeric,claves.outliers.normales,colnames(mydata.numeric)[indice.columna])






###########################################################################
# BoxPlot
###########################################################################


# Vemos el diagrama de caja 

# Para ello, podr?amos usar la funci?n boxplot, pero ?sta no muestra el outlier en la columna mpg :-(
# Por lo tanto, vamos a usar otra funci?n. Esta es la funci?n geom_boxplot definida en el paquete ggplot
# En vez de usarla directamente, llamamos a la siguiente funci?n:
# MiBoxPlot_IQR_Univariate_Outliers = function (datos, indice.de.columna, coef = 1.5)
# Esta funci?n est? definida en el fichero de funciones A3 que ha de cargar previamente.
# Esta funci?n llama internamente a geom_boxplot

# Una vez que la hemos llamado con mydata.numeric y con indice.columna, la volvemos
# a llamar pero con los datos normalizados.
# Lo hacemos para resaltar que el Boxplot es el mismo ya que la normalizaci?n
# no afecta a la posici?n relativa de los datos 





# COMPLETAR  
MiBoxPlot_IQR_Univariate_Outliers(mydata.numeric,indice.columna)





###########################################################################
# C?mputo de los outliers IQR con funciones propias
###########################################################################

# En este apartado hacemos lo mismo que antes, pero llamando a funciones que est?n dentro de !Outliers_A3_Funciones.R :

# vector_es_outlier_IQR = function (datos, indice.de.columna, coef = 1.5)  
# datos es un data frame y coef es el factor multiplicativo en el criterio de outlier,
# es decir, 1.5 por defecto para los outliers normales y un valor mayor para outliers extremos (3 usualmente)
# -> devuelve un vector TRUE/FALSE indicando si cada dato es o no un outlier


# vector_claves_outliers_IQR = function(datos, indice, coef = 1.5)
# Funci?n similar a la anterior salvo que devuelve los ?ndices de los outliers








###########################################################################
# BoxPlot
###########################################################################


# Mostramos los boxplots en un mismo gr?fico.
# Tenemos que usar los datos normalizados, para que as? sean comparables


# Llamamos a la funci?n  MiBoxPlot_Juntos
# MiBoxPlot_juntos  = function (datos, vector_TF_datos_a_incluir)  
# Pasamos mydata.numeric como par?metro a datos.
# Si no pasamos nada como segundo par?metro, se incluir?n todos los datos en el c?mputo.
# Esta funci?n normaliza los datos y muestra, de forma conjunta, los diagramas de cajas
# As?, podemos apreciar qu? rango de valores toma cada outlier en las distintas columnas.

# Para etiquetar los outliers en el gr?fico
# llamamos a la funci?n MiBoxPlot_juntos_con_etiquetas 




# COMPLETAR  

MiBoxPlot_juntos(mydata.numeric)





###########################################################################
###########################################################################
# AMPLIACI?N
###########################################################################
###########################################################################



###########################################################################
# TODO LO QUE HAY A PARTIR DE AHORA ES DE AMPLIACI?N
# RESU?LVALO S?LO CUANDO TERMINE EL RESTO DE FICHEROS
# POR LO TANTO, PASE AHORA A RESOLVER EL SIGUIENTE FICHERO

# ESTA PARTE DE AMPLIACI?N ES OBLIGATORIO RESOLVERLA EN EL CASO DE QUE HAGA 
# EL TRABAJO DEL CURSO SOBRE LA PARTE DE ANOMAL?AS
###########################################################################




###########################################################################
# Trabajamos con varias columnas simult?neamente
###########################################################################


# Los outliers siguen siendo univariate, es decir, con respecto a una ?nica columna
# Pero vamos a aplicar el proceso anterior de forma autom?tica a todas las columnas

# Vamos a obtener los ?ndices de aquellos registros que tienen un outlier en alguna de las columnas
# As? pues, vamos a construir la siguiente variable:

# indices.de.outliers.en.alguna.columna -> 
# Contiene los ?ndices de aquellos registros que tengan un valor an?malo en cualquiera de las columnas

# Para ello, hay que aplicar la funci?n vector_claves_outliers_IQR sobre cada una de las columnas
# Para hacer esto autom?ticamente con todas las columnas, usamos sapply:
#   El primer argumento de sapply ser? el rango de las columnas que vamos a barrer, es decir, 1:ncol(mydata.numeric)
#   El segundo argumento de sapply ser? la funci?n a aplicar sobre el primer argumento, es decir, vector_claves_outliers_IQR 
#   Consulte la ayuda para obtener m?s informaci?n sobre sapply
# Como esta funci?n devuelve una lista, al final, tenemos una lista de listas.
# Para "desempaquetar" el resultado, usamos la funci?n unlist
# El resultado lo guardamos en
# indices.de.outliers.en.alguna.columna
# [1] 20 31 15 16 17  9

# El anterior resultado nos quiere decir que las filas con claves 20, 31, 15, 16, 17 y 9 
# tienen un outlier en alguna de sus columnas


# Mostramos los valores normalizados de los registros que tienen un valor an?malo en cualquier columna 
# Pero mostramos los valores de todas las columnas 
# (no s?lo la columna con respecto a la cual cada registro era un valor an?malo)

#                       mpg       cyl       disp         hp       drat          wt        qsec
# Toyota Corolla       2.2912716 -1.224858 -1.2879099 -1.1914248  1.1660039 -1.41268280  1.14790999
# Maserati Bora       -0.8446439  1.014882  0.5670394  2.7465668 -0.1057878  0.36051645 -1.81804880
# Cadillac Fleetwood  -1.6078826  1.014882  1.9467538  0.8504968 -1.2466598  2.07750476  0.07344945
# Lincoln Continental -1.6078826  1.014882  1.8499318  0.9963483 -1.1157401  2.25533570 -0.01608893
# Chrysler Imperial   -0.8944204  1.014882  1.6885616  1.2151256 -0.6855752  2.17459637 -0.23993487
# Merc 230             0.4495434 -1.224858 -0.7255351 -0.7538702  0.6049193 -0.06873063  2.82675459

# Vemos, por ejemplo, que el Toyota se dispara (por arriba) en mpg pero no tanto en el resto de columnas
# El Maserati se dispara en hp (por arriba) y algo menos en qsec (por abajo)



# COMPLETAR  







#----------------------------------------------------------------------
# Hacemos los mismo pero llamando a una funci?n proporcionada en el fichero A2

# Vuelva a calcular el valor de la variable indices.de.outliers.en.alguna.columna 
# llamando a la funci?n
# vector_claves_outliers_IQR_en_alguna_columna = function(datos, coef = 1.5)
# para comprobar que obtiene el mismo resultado
# coef es 1.5 para los outliers normales y hay que pasarle 3 para los outliers extremos

# [1] 20 31 15 16 17  9






# COMPLETAR  






###########################################################################
# ?ndices de aquellos registros que tienen un outlier en alguna de las columnas
###########################################################################


# Obtenemos la siguiente matriz:
# frame.es.outlier -> matriz de T/F en la que por cada registro (fila), nos dice si
#                     es un outlier IQR en la columna correspondiente
# PAra ello, tenemos que aplicar la funci?n vector_es_outlier_IQR sobre cada una de las columnas
# Para ello, usamos sapply:
#   El primer argumento de sapply ser? el rango de las columnas que vamos a barrer, es decir, 1:ncol(mydata.numeric)
#   El segundo argumento de sapply ser? la funci?n a aplicar sobre el primer argumento, es decir, vector_es_outlier_IQR 
#   Consulte la ayuda para obtener m?s informaci?n sobre sapply

# 
#       [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7] 
# [1,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [2,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [3,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [4,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [5,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [6,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [7,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [8,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [9,] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  <- El registro 9 tiene un valor an?malo en la columna 7
# [10,] ...............

# Puede observar que en la columna 6 hay tres outliers, en las filas con ?ndices 15, 15, 17.

# Construyamos la variable:
# numero.total.outliers.por.columna -> N?mero de outliers que hay en cada variable (columna)

# Para ello, sobre la matriz anterior, usamos apply sobre la dimensi?n 2 (las columnas) y aplicamos la funci?n sum
# Consulte la ayuda para obtener m?s informaci?n sobre apply

# [1] 1 0 0 1 0 3 1







# COMPLETAR  














