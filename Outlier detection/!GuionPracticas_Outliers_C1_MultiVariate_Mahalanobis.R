# M?ster -> Detecci?n de anomal?as
# Juan Carlos Cubero. Universidad de Granada

###########################################################################
# MULTIVARIATE OUTLIERS -> Multivariate Normal Distribution 
# -> Mahalanobis
###########################################################################


###########################################################################
# RESUMEN:

# El objetivo es calcular los outliers multivariantes
# Un outlier multivariante tendr?:
# - o bien un valor anormalmente alto o bajo en alguna de las variables
# - o bien una combinaci?n an?mala de valores en 2 o m?s variables
#   ?stos ?ltimos ser?n los m?s interesantes de detectar.

# En este apartado se van a utilizar t?cnicas estad?sticas param?tricas.
# En nuestro caso, se necesita que los datos est?n distribuidos seg?n una normal 
# multivariante, aunque si las distribuciones de cada variable son unimodales
# tambi?n se suele aplicar este tipo de tests
# 
# Bajo estas premisas, se usa la distancia de Mahalanobis 
# para medir c?mo de alejado est? cada dato al centro de la distribuci?n,
# es decir, hasta qu? punto es un outlier multivariante.
# 
# Podemos plantear dos tipos de tests:
# a) H0: El dato con m?xima distancia de Mahalanobis no es un outlier 
#    H1: El dato con m?xima distancia de Mahalanobis es un outlier 
#    En este caso usar?amos el alpha usual (0.05)
# b) H0: No hay outliers
#    H1: Hay al menos un outlier
#    En este caso usar?amos un alpha penalizado para tener en cuenta el error FWER
#    (Ver p?gina 96 de las transparencias)
# En el test b) ser? m?s dif?cil rechazar debido a la penalizaci?n del nivel de significaci?n

# No hay que normalizar los datos ya que la distancia de Mahalanobis est?
# dise?ada, precisamente para evitar el problema de la escala.

# Conjunto de datos:


mis.datos.numericos =  mtcars[,-c(8:11)]  #
mis.datos.numericos.normalizados = scale(mis.datos.numericos)
nivel.de.significacion = 0.05
nivel.de.significacion.penalizado = 1 - ( 1 - nivel.de.significacion) ^ (1 / nrow(mis.datos.numericos))  # Transparencia 96



###########################################################################
# Obtenci?n de los outliers multivariantes

# Transparencia 97

# Usaremos el paquete CerioliOutlierDetection 

# Obtiene los outliers calculando las distancias de Mahalanobis 
# La estimaci?n de la matriz de covarianzas es la estimaci?n robusta seg?n MCD 
# (minimum covariance determinant)
# La distribuci?n del estad?stico es la obtenida en Hardin-Rocke o  Green and Martin 
# (ver documentaci?n del paquete CerioliOutlierDetection)


# Establecemos la semilla para el m?todo iterativo que calcula MCD 
# IMPORTANTE: Para que el resultado sea el mismo en todas las ejecuciones, 
# siempre hay que establecer la semilla antes de lanzar la funci?n correspondiente.

set.seed(12)  

# Llamamos a cerioli2010.fsrmcd.test pas?ndole como primer par?metro nuestro dataset
# Pasamos como par?metro a signif.alpha el valor 0.05. 
# Tal y como indica la documentaci?n del paquete Cerioli, al utilizar un valor
# de significaci?n "t?pico" de los test de hip?tesis como es 0.05, el test que
# vamos a aplicar es del tipo a)
# a) H0: El dato con m?xima distancia de Mahalanobis no es un outlier 
#    H1: El dato con m?xima distancia de Mahalanobis es un outlier 
#    En este caso usar?amos el alpha usual (0.05)

# Guardamos el resultado en la variable 
# cerioli
# Accedemos a cerioli$outliers para obtener un vector de T/F indicando
# si cada dato es o no un outlier. Guardamos el resultado en la variable
# is.outlier.cerioli
# A partir de ella, obtenemos un vector de ?ndices de los marcados como outliers:
# Nos debe salir:

# 9  31

# Nos salen dos outliers porque el procedimiento realiza un test por separado
# a cada valor del dataset. Esto puede confundir ya que al fijar un nivel de significaci?n de 0.05
# s?lo podemos fijarnos en el valor m?s extremo (es un test de tipo a))
# Por lo tanto, para saber cu?l es el m?s extremo de los dos 
# tenemos que ver los valores de la distancia de Mahalanobis
# (realmente es una distancia modificada -ponderada- por los autores del paquete)
# ordenarlos y ver a qui?n corresponde el m?ximo.
# Para ello, construimos la variable:
# dist.mah.ponderadas = cerioli$mahdist.rw   # Valores de la distancia de Mahalanobis ponderada
# Ordenamos decrecientemente estas distancias y obtenemos los ?ndices correspondientes en la variable
# indices.dist.mah.ponderadas
# Nos debe salir:

# [1] 31  9 29 17 28 ......

# Por lo tanto, el mayor valor de distancia de Mahalanobis ponderada es el 31.
# Por lo tanto, el test de tipo a) rechazar?a la hip?tesis de que el 31
# no es un outlier y lo aceptamos como outlier. 

# Cabr?a ahora responder a la pregunta:
# ?Es un outlier porque tiene un valor muy alto en alguna variable?
# ?O es un outlier "puro" (m?s interesante) porque tiene una combinaci?n
# anormal de variables?
# Para ello, vamos a obtener los datos normalizados de dicho valor. Debe salir:

#                mpg       cyl       disp         hp       drat          wt      qsec
# Maserati Bora -0.8446439  1.014882  0.5670394  2.7465668 -0.1057878  0.36051645 -1.818049

# En este caso, parece que es porque tiene un valor de hp (horse power)
# muy alto (2.746 como valor normalizado)
# En el pr?ximo script de las pr?cticas volveremos sobre este asunto.


# Pasamos ahora a ejecutar un test del tipo b):
# b) H0: No hay outliers
#    H1: Hay al menos un outlier
#    En este caso usar?amos un alpha penalizado para tener en cuenta el error FWER
#    (Ver p?gina 96 de las transparencias)

# Tal y como indica la documentaci?n del paquete, para lanzar un test del tipo b)
# debemos lanzar el test fsrmcd pero con un nivel de significaci?n penalizado.
# As? pues, lanzamos fsrmcd con nivel.de.significacion.penalizado
# Nos saldr? que no podemos rechazar la hip?tesis nula de que no hay outliers
# Tal y como dijimos al principio, esto se produce porque al estar
# contrastando de forma conjunta muchas hip?tesis, perdemos potencia en el test
# Con un test de tipo a) pudimos establecer que el 31 era un outlier pero si
# usamos un test de tipo b) comparando si cada uno de los datos es un outlier,
# perdemos potencia y no podemos rechazar 





# COMPLETAR  


cerioli=cerioli2010.fsrmcd.test(mis.datos.numericos)
cerioli

# Nos salen dos outliers porque el procedimiento realiza un test por separado
# a cada valor del dataset. Esto puede confundir ya que al fijar un nivel de significaci?n de 0.05
# s?lo podemos fijarnos en el valor m?s extremo (es un test de tipo a))
# Por lo tanto, para saber cu?l es el m?s extremo de los dos 
# tenemos que ver los valores de la distancia de Mahalanobis
# (realmente es una distancia modificada -ponderada- por los autores del paquete)
# ordenarlos y ver a qui?n corresponde el m?ximo.
# Para ello, construimos la variable:
# dist.mah.ponderadas = cerioli$mahdist.rw   # Valores de la distancia de Mahalanobis ponderada
# Ordenamos decrecientemente estas distancias y obtenemos los ?ndices correspondientes en la variable
# indices.dist.mah.ponderadas
# Nos debe salir:

# [1] 31  9 29 17 28 ......

dist.mah.ponderadas<-cerioli$mahdist.rw
indexed<-cbind(dist.mah.ponderadas,c(1:32))
indexed<-as.data.frame(indexed)
?order
indices.dist.mah.ponderadas<-indexed[do.call(order, args=c(indexed,decreasing=TRUE)),][2]


mis.datos.numericos.normalizados[indices.dist.mah.ponderadas[1,1],]

# Pasamos ahora a ejecutar un test del tipo b):
# b) H0: No hay outliers
#    H1: Hay al menos un outlier
#    En este caso usar?amos un alpha penalizado para tener en cuenta el error FWER
#    (Ver p?gina 96 de las transparencias)

# Tal y como indica la documentaci?n del paquete, para lanzar un test del tipo b)
# debemos lanzar el test fsrmcd pero con un nivel de significaci?n penalizado.
# As? pues, lanzamos fsrmcd con nivel.de.significacion.penalizado
# Nos saldr? que no podemos rechazar la hip?tesis nula de que no hay outliers
# Tal y como dijimos al principio, esto se produce porque al estar
# contrastando de forma conjunta muchas hip?tesis, perdemos potencia en el test
# Con un test de tipo a) pudimos establecer que el 31 era un outlier pero si
# usamos un test de tipo b) comparando si cada uno de los datos es un outlier,
# perdemos potencia y no podemos rechazar 

cerioli2010.fsrmcd.test(mis.datos.numericos,signif.alpha = nivel.de.significacion.penalizado)









