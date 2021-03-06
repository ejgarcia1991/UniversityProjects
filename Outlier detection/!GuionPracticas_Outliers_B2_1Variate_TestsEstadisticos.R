# M�ster -> Detecci�n de anomal�as
# Juan Carlos Cubero. Universidad de Granada

###########################################################################
# UNIVARIATE STATISTICAL OUTLIERS -> 1-variate Normal Distribution
###########################################################################






###########################################################################
# TODO ESTE FICHERO (B2) ES DE AMPLIACI�N
# RESU�LVALO S�LO CUANDO TERMINE EL RESTO DE FICHEROS 
# POR LO TANTO, PASE AHORA A RESOLVER EL SIGUIENTE FICHERO (C1)

# ESTA PARTE DE AMPLIACI�N ES OBLIGATORIO RESOLVERLA EN EL CASO DE QUE HAGA 
# EL TRABAJO DEL CURSO SOBRE LA PARTE DE ANOMAL�AS
###########################################################################








###########################################################################
# RESUMEN:

# El objetivo es el mismo que el indicado en el fichero B1, es decir, encontrar
# outliers bajo la hip�tesis de normalidad
# La diferencia es que ahora vamos a aplicar un test estad�stico
# para poder declarar que el valor m�s alejado de la media es un outlier
# con una significaci�n estad�stica establecida a priori

# Se aplicar�n los siguientes tests:

# Grubbs' test. Normal 1-dim. 1 �nico outlier
# grubbs.test {outliers}

# Rosner's test. Normal 1-dim. <= k outliers.
# rosnerTest {EnvStats}
###########################################################################



###########################################################################
# Conjuntos de datos 
###########################################################################


datos.con.un.outlier           = c(45,56,54,34,32,45,67,45,67,140,65)
mydata.numeric = datos.con.un.outlier


###########################################################################
# Test de Grubbs
###########################################################################

# Transparencia 83


###########################################################################
# datos.con.un.outlier

# Mostramos el histograma de mydata.numeric usando la funci�n hist
# y un gr�fico de puntos con la funci�n plot
# Observamos que hay un dato con un valor extremo

# -------------------------------------------------------------------------


# Aplicamos el test de Grubbs sobre datos.con.un.outlier
# Usamos la funci�n grubbs.test (two.sided = TRUE)
# Guardamos el resultado en test.de.Grubbs y vemos el p.value correspondiente

# [1] 0.001126431  

# Este resultado es significativo con los valores de alpha usuales 0.025, 0.01

# -------------------------------------------------------------------------

# El test de Grubbs es significativo por lo que se concluye que hay un �NICO outlier
# El valor que toma (140) los podr�amos obtener a trav�s de la funci�n outlier del paquete outliers
# pero �ste no nos dice cu�l es el �ndice correspondiente (10).
# Por lo tanto, calculamos manualmente cu�l es el �ndice de aquel registro
# que m�s se desv�a de la media de la columna correspondiente.
# Tendremos que usar las funciones abs(valor absoluto), mean(media) y order (para ordenar)
# Tenga en cuenta que el resultado de order es un conjunto de �ndices y no de valores originales
# El resultado lo guardamos en las siguientes variables:
# indice.de.outlier.Grubbs
# valor.de.outlier.Grubbs

# [1] 10
# [1] 140

# -------------------------------------------------------------------------

# Ahora que sabemos el �ndice del outlier, podemos usar la funci�n MiPlot_Univariate_Outliers
# Esta funci�n muestra un plot similar al que ya hab�amos mostrado, pero usa el color rojo para mostrar el outlier
# Los par�metros son: el conjunto de datos, los �ndices de los outliers (s�lo uno en este caso) y el t�tulo a mostrar
# MiPlot_Univariate_Outliers = function (datos, indices_de_Outliers, titulo)

# Resultado:

# N�mero de datos: 11
# �Qui�n es outlier?: FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE TRUE FALSE





# COMPLETAR  






###########################################################################
# Realizamos el mismo proceso anterior empaquetado en una funci�n 
###########################################################################


# Llamamos a la funci�n MiPlot_resultados_TestGrubbs
# MiPlot_resultados_TestGrubbs = function(datos)
# Esta funci�n realiza todo el proceso de aplicar el test de Grubbs tal y como hemos hecho anteriormente
# Tambi�n muestra los resultados: para ello, la funci�n llama directamente a MiPlot_Univariate_Outliers
# El par�metro a pasar a la funci�n MiPlot_resultados_TestGrubbs es el conjunto de datos
# 
# p.value: 0.001126431
# �ndice de outlier: 10
# Valor del outlier: 140
# N�mero de datos: 11
# �Qui�n es outlier?: FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE TRUE







# COMPLETAR  







###########################################################################
# Masking/Swamping. Transparencias 86-88
###########################################################################

# Vamos a  aplicar el mismo proceso con otro conjunto de datos, para ver 
# c�mo se produce el "masking"
# En el caso de que vaya a hacer el trabajo de Anomal�as, s�lo ejecute 
# los scripts siguientes con  el conjuntos de datos datos.con.dos.outliers.masking
# y no los aplique a su conjunto de datos.


datos.con.dos.outliers.masking = c(45,56,54,34,32,45,67,45,67,154,125,65)


# Mostramos un gr�fico de puntos con la funci�n plot
# Vemos que hay dos outliers

# Aplicamos el test de Grubbs sobre datos.con.dos.outliers.masking

# [1] 0.05614091

# El resultado no es significativo con ninguno de los valores de alpha usuales (<= 0.05)
# Sin embargo, hay dos outliers. (125, 154). 
# La raz�n es que se ha producido un efecto de "masking"  
# Ning�n outlier es detectado por Grubbs :-(




# COMPLETAR  







# Hay tests para detectar un n�mero exacto de k outliers, pero no son muy �tiles
# Mejor usamos un test para detectar un n�mero menor o igual que k outliers (Rosner)

# Aplicamos el Test de Rosner (rosnerTest) con k=4 sobre datos.con.dos.outliers.masking
# Nos dar� un aviso ocasionado por tener pocos datos
# Guardamos el resultado en test.de.rosner 
# El test ordena los valores de mayor a menor distancia de la media y lanza el test de hip�tesis
# para ver si hay menos de k=4 outliers.

# Imprimimos los siguientes campos:
#   test.de.rosner$all.stats$Outlier 
#     Es un vector de 4 boolean. 
#     Nos indica si son considerados outliers los 4 valores que m�s se alejan de la media
#     En este caso:
#     [1]  TRUE  TRUE FALSE FALSE
#     Los dos primeros son TRUE y el resto FALSE => El test indica que hay dos outliers :-)
#   
#   test.de.rosner$all.stats$Obs.Num
#     Es un vector con los cuatro �ndices de los 4 valores que
#     m�s se alejan de la media
#     En este caso:
#     [1]  10    11   5     4

# Construimos el vector con los �ndices de los que son outliers (10, 11)
# y se lo pasamos como par�metro a la funci�n
# MiPlot_Univariate_Outliers
# MiPlot_Univariate_Outliers = function (datos, indices_de_Outliers, titulo){

# N�mero de datos: 12
# �Qui�n es outlier?: FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE TRUE TRUE FALSE




# COMPLETAR  







#######################################################################

# La funci�n 
# MiPlot_resultados_TestRosner = function(datos)
# hace directamente las anteriores tareas, es decir, lanza el test y dibuja el plot.
# Lanzamos esta funci�n con el dataset datos.con.dos.outliers.masking 
# y comprobamos que ofrece los resultados vistos anteriormente



# COMPLETAR  



