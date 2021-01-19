hip  <-read.table("http://astrostatistics.psu.edu/datasets/HIP_star.dat", header=T,fill=T) 

#Una vez descargado comprueba la dimensión y los nombres de las columnas del dataset. ¿Qué dimensión tiene? 
#¿qué tipo de datos alberga? (e.g. 4 variables numéricas continuas) 
str(hip)
#2719 valores, 9 variables, 8 son variables numéricas continuas de punto flotante y una es numérica de valores enteros.
#Muestra por pantalla la columna de la variable RA 
hip["RA"]
#Calcula las tendencias centrales de todos los datos del dataset (mean, media) utilizando la function apply
apply(hip,2,mean,na.rm=TRUE)
apply(hip,2,median,na.rm=TRUE)
#Haz lo mismo para las medidas de dispersión mínimo y máximo. ¿Seria posible hacerlo con un único comando?¿Que hace la función range() 
apply(hip,2,range,na.rm=TRUE)
#Sin embargo las medidas mas populares de dispersión son la varianza (var()), su desviación standard (sd()) 
#y la desviación absoluta de la mediana o MAD. Calcula estas medidas para los valores de RA
var(hip["RA"])
sd(hip[,3])
mad(hip["RA"],na.rm=TRUE)
#* Imagina que quieres calcular dos de estos valores de una sola vez. ¿Te serviría este código? 
#f = function(x) c(median(x), mad(x))   
#f(hip[,3]) 

f = function(x) c(median(x), mad(x))   
f(hip[,3]) 
#Sí, devuelve los valores.
# ¿Cuál sería el resultado de aplicar apply(hip,2,f)? 
apply(hip,2,f)
# Se aplica la función que devuelve la mediana y la desviación absoluta de la mediana a cada variable,
# pero al no añadir na.rm=TRUE una columna se queda con valores NA

#Vamos a medir la dispersión de la muestra utilizando el concepto de cuartiles. 
#El percentil 90 es aquel dato que excede en un 10% a todos los demás datos. 
#El cuartil (quantile) es el mismo concento, solo que habla de proporciones en vez de porcentajes. 
#De forma que el percentil 90 es lo mismo que el cuartil 0.90. 
#La mediana “median” de un dataset es el valor más central, en otras palabras exactamente 
#la mitad del dataset excede la media. Calcula el cuartil .10 y .50 para la columna RA del 
#dataset hip. Sugerencia: quantile() 

quantile(hip[,3],probs=c(0.1, 0.5))

#Los cuantiles 0.25 y 0.75 se conocen como el  first quartile y el third quartile, respectivamente.
#Calcula los cuatro cuartiles para RA con un único comando. 

quantile(hip[,3],probs=seq(0.25, 1,0.25))

#Otra medida de dispersion es la diferencia entre el primer y el tercer cuartil conocida como rango intercuartil (IQR) Inter Quantile Range. 
#¿Obtienes ese valor con la función summary()? 
summary(hip[,3])  
#Obtengo el 1er cuartil y el 3er cuartil, bastaría con restar para obtener el valor.
  
  
#Hasta ahora has ignorado la presencia de  valores perdidos NA. 
#La función any() devuelve TRUE si se encuentra al menos un TRUE en el vector que damos como argumento. 
#Su combinación con is.na es muy útil. ¿qué obtienes cuando ejecutas el siguiente comando? ¿Cómo lo interpretas? 
hasNA = function(x) any(is.na(x))  
apply(hip,2,hasNA)    
#Entiendo que la columna B.V tiene al menos un valor NA que debe ser tratado antes de aplicar funciones sensibles a NA

#Prueba a ejecutar el siguiente comando. 
min(B.V) 
#Error, object B.V not found
min(hip["B.V"])
#NA, tal como se esperaba
hip1 = na.omit(hip) 
#Como has observado  nos devuelve NA para toda la columna,  normalmente querríamos poder usar la función sobre el resto de datos que no son NA: Para ello podemos utilizar la función na.omit. 
#¿Que ocurre cuando lo hacemos?. Usando apply calcula la media para hip. Intenta calcular la media de forma que solo cambie la de B.V cuando ignores los valores NA. 
apply(hip1,2,mean)
apply(hip,2,mean,na.rm=TRUE)

#Obten una idea aproximada de tus datos mediante la creación de un boxplot del hip dataset 
boxplot(hip)
boxplot(hip[,2:9])
#Los datos tienen una variedad muy elevada, ya que hay valores muy pequeños y valores muy grandes.

#Crea un scatterplot que te compare los valores de RA y DE. 
#Representa los puntos con el símbolo ‘.’ Y que estos puntos sean de color rojo si DE excede de 0.Sugerencia ifelse() 
plot(hip[,3:4],col=ifelse(hip[,4]>0,"red","blue"),pch='.')

#Haz un scatterplot de RA y pmRA. ¿Ves algún patrón? 
plot(hip[,c(3,6)],pch='.')
#Viene Batman!!! es un murcielago!!!

#En vez de crear los plots por separado para cada par de columnas, 
#hazlos con un solo comando con el scatterplot matrix 
pairs(hip[,c(3,5,6)])
#no se puede hacer pairs a todo el dataset por la diferencia de tamaño entre las variables, así que escogí las de los ejercicios anteriores

#Para poder acceder a las variables por su nombre usa attach(hip).
#Vamos a seleccionar las estrellas Hyadas del dataset aplicando los siguientes filtros: 
attach(hip)
hip %>% filter(RA>50.0 & RA<100.0) %>% filter(DE>0 & DE<25)  %>% filter(pmRA>90 & pmRA<130) %>% filter(pmDE<60 & pmDE>-10) %>% filter(e_Plx<5) %>% filter(Vmag >4 | B.V <0.2)

#Crea un nuevo dataset con la aplicación de estos filtro. El Nuevo dataset se llama hyades. 
#¿Que dimensiones tiene? Grafica un scatterplot de Vmag vs B.V 
hyades<-hip %>% filter(RA>50.0 & RA<100.0) %>% filter(DE>0 & DE<25)  %>% filter(pmRA>90 & pmRA<130) %>% filter(pmDE<60 & pmDE>-10) %>% filter(e_Plx<5) %>% filter(Vmag >4 | B.V <0.2)
str(hyades)
#6 objetos con 9 columnas
plot(hyades[c(2,9)])

#* Vamos a utilizar el ejemplo del dataset iris que está incluido en la distribución de R. Este dataset fue creado por Douglas Fisher.  Consta de tres clases y tipos de 3 clases de tipos de flores: 
#_setosa_ 
#_virginica_ 
#_versicolor_ 
#Cada una de ellas con cuatro atributos: 
#sepal width 
#sepal length 
#petal width 
#petal length 


#Inspecciona las primeras filas del dataset y calcula el summary() del mismo con cada atributo del dataset
iris[seq(1,5),]
summary(iris)
#Crea un histograma de petal.width , teniendo en cuenta que el numero de bins es variable fija este a 9. 
#Añádele color y nombres al eje x "Petal Width"y al gráfico dale el nombre de  "Histogram of Petal Width". Crea un histograma para cada variable 
hist(iris[,4],breaks=9,main="Histogram of Petal Width",xlab="Petal Width",col="green")
hist(iris[,1])
hist(iris[,2])
hist(iris[,3])

#Crea los cuartiles del dataset
quantile(iris[,1],probs=seq(0.25, 1,0.25))
quantile(iris[,2],probs=seq(0.25, 1,0.25))
quantile(iris[,3],probs=seq(0.25, 1,0.25))
quantile(iris[,4],probs=seq(0.25, 1,0.25))


#Representa en un boxplot la variable de ancho de hoja dependiendo del tipo de hoja que tengan
boxplot(iris[,4]~iris[,5])
#Crea los cuartiles para cada tipo de iris y represéntalos en un plot como líneas cada una de un color
plot(quantile(iris[,5],probs=seq(0.25, 1,0.25)))
quantile(iris[,2],probs=seq(0.25, 1,0.25))
quantile(iris[,3],probs=seq(0.25, 1,0.25))
quantile(iris[,4],probs=seq(0.25, 1,0.25))

#Crea los boxplot de la longitud del pétalo en función de la especie de Iris.
boxplot(iris[,3]~iris[,5])
#Compara con scatter plots las variables entre sí.
pairs(iris)
#Crea una nueva columna llamada proporción que es el ratio entre Sepal.Length y Sepal.Width. 
#Podeis hacerlo en R base o usando el paquete dplyr. Los que no lo conozcan tienen información al final del documento 
iris_con_columna<-mutate(iris,iris[,3]/iris[,4])
colnames(iris_con_columna)[6]<-"Proporción"
#El conjunto de datos “swiss” contiene una medida estandarizada de fecundidad y varios indicadores socioeconómicos para cada una de las 47 provincias francófonas de Suiza.
#¿Qué diagrama dibujaría para mostrar la distribución de todos los valores? ¿Qué conclusiones sacarías?  
boxplot(swiss)
#Un boxplot, veo que la media de fertilidad y agricultura es muy alta, aunque varía mucho la de agricultura
#La educación es baja, muy concentrada pero con valores outliers muy notables, la tasa de mortalidad tiene una fluctuación muy baja, concentrada sobre 20
#Y por último es notable la presencia del catocilismo, casi todos los datos están contenidos entre el primer y el 3er cuartil

#Dibuje gráficos para cada variable. ¿Qué puede concluir de las distribuciones con respecto a su forma y posibles valores atípicos?
hist(swiss[,1]) #Muy alta fertilidad en 39 ciudades, con solo 7 por debajo del 60%
hist(swiss[,2]) #Muy equilibrada en general, solo dos ciudades no poseen casi agricultura
hist(swiss[,3]) #No hay mucho que decir, es una curva bastante normal, un poco concentrada cerca de 20.
hist(swiss[,4]) #La educación es horrible en la mayoría de las ciudades, existiendo solo 2 ciudades con más de un 30% de personajes con educación superior a la primaria
hist(swiss[,5]) #20 ciudades no poseen casi católicos, mientras que 15 poseen más de un 90%, las restantes se encuentran con valores variados en el medio
hist(swiss[,6]) #La mortalidad infantil está muy concentrada en 20, solo 2 ciudades con valores extremos.


#Dibuja un diagrama de dispersión de Fertilidad frente a % Catholic. ¿Qué tipo de áreas tienen las tasas de fertilidad más bajas?  
plot(swiss[,1],swiss[,5],xlab="fertility",ylab="Catholic")
#los que se encuentran entre 40 y 60

#¿Qué tipo de relación existe entre las variables Educación y Agricultura? 
plot(swiss[,4],swiss[,2],xlab="Education",ylab="Agriculture")
#Como era de esperarse, mientras más baja la educación, más probable de trabajar en la agricultura (ciudades campestres y pueblos)

#El conjunto de datos de aceites de oliva es bien conocido y se puede encontrar en varios paquetes, por ejemplo, como aceitunas en extracat.. La fuente original de los datos es el artículo [Forina et al., 1983].  
install.packages("extracat")
install.packages("olive")

#FALLAN, package ‘extracat’ is not available (for R version 3.6.1), lo mismo para todos los dataset de "olive" que encontré en internet
#Dibuje un scatterplot  de las ocho variables continuas. ¿Cuáles de los ácidos grasos están fuertemente asociados positivamente y cuáles fuertemente asociados negativamente?  
#¿Hay valores atípicos u otras características que valga la pena mencionar? 

#El conjunto de datos se llama Lanza del paquete HSAUR2.  
install.packages("HSAUR2")
library(HSAUR2)
str(Lanza)
#Se informan los datos de cuatro estudios. Dibuje un diagrama para mostrar si los cuatro estudios son igualmente grandes.
plot(Lanza[,1]) #Son bastante parecidos, excepto el 4 que es mucho menor

#El resultado se mide por la clasificación de la variable con puntuaciones de 1 (mejor) a 5 (peor). ¿Cómo describirías la distribución? 
plot(Lanza[,3]) #Excelente, cerca de un 40% tiene puntuación 1

#El paquete vcdExtra incluye datos de un viejo estudio de cáncer de mama sobre la supervivencia o muerte de 474 pacientes.  
install.packages("vcdExtra")
library(vcdExtra)
#Convierta los datos en un data frame y dibuje gráficos para comparar las tasas de supervivencia, primero, por grado de malignidad y, en segundo lugar, por centro de diagnóstico.
df<-data.frame(vcdExtra::Cancer)

#No entiendo, quiere esto?
df1<-df[,1]=="Surv"
df1<-df[df1,]
plot(df1[,4]~df1[,2])
plot(df1[,4]~df1[,3])

#O un barplot de 3 variables que requiere mucho más trabajo pues tendría que agrupar los datos según el elemento que no vaya a usar
library(dplyr)
df1<- df %>% group_by(Grade,Survival) %>% summarize(sum(Freq))
df1<-data.frame(df1)
dat <- matrix(df1[,3],nrow=2)
colnames(dat) <- c("Malignant","Benign")
rownames(dat) <- c("Died","surv")
barplot(dat,beside=T,legend=rownames(dat))

df1<- df %>% group_by(Center,Survival) %>% summarize(sum(Freq))
df1<-data.frame(df1)
dat <- matrix(df1[,3],nrow=2)
colnames(dat) <- c("Boston","Glamorgan")
rownames(dat) <- c("Died","surv")
barplot(dat,beside=T,legend=rownames(dat))


#¿Qué diagrama dibujaría para comparar las tasas de supervivencia tanto por grado de malignidad como por centro de diagnóstico? ¿Importa el orden de las variables explicativas?
#Supongo que como en el ejercicio anterior realizaría una agrupación primero y luego un barplot de 3 variables. El orden de los datos importa a la hora de crear la matriz y de colocar los nombres en si en el gráfico

#Dataset Crabs (del paquete MASS) [Venables y Ripley, 2002]. Los autores inicialmente se transforman a una escala logarítmica y luego escriben que: 
install.packages("MASS")
library(MASS)
crabs
#“The data are very highly correlated and scatterplot matrices and brush plots [i.e. interactive graphics] are none too revealing.”.  
#Utilizando gráficos generales, comente si la transformación logaritmica fue una buena idea y si está de acuerdo con su afirmación sobre las correlaciones.
plot(log1p(crabs[3:8]))
plot(crabs[3:8])

#la diferencia es realmente casi imperceptible, la transformación logaritmica no ayuda en este caso
