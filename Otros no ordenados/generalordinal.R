#Se cargan las librerías necesaria para el clasificador usado, en los ejemplos se usa RWeka
library(RWeka)
library(caret)

#Se define la semilla de aleatoriedad para asegurar consistencia en los elementos aleatorios
set.seed(2)

#Esta función recibe un dataframe ordinal, el clasificador que se espera que se use como una cadena
#de caracteres y los parametros que se desea que se pasen al clasificador, se asume que la
#última columna es la clase a predecir
#Ejemplo de uso 1: modelos<- ordinalClassficatorModelos(train,"JRip")
#Ejemplo de uso 2: modelos<- ordinalClassficatorModelos(train,"kknn",test,k=13)
#Ejemplo de uso 3: modelos<- ordinalClassficatorModelos(ordinalDf,"J48",subset=train)

ordinalClassificatorModels<-function(df,classifier,...){
  siz<-ncol(df) #Se almacena en una variable la posición de la última variable por comodidad
  clases<-as.integer(unique(df[,ncol(df)])) #Se almacena un vector conteniendo las clases de la variable a predecir
  aux<-vector("list", length(clases)-1) #Creamos una lista con tamaño equivalente al número de clases -1 para los data frames binarios
  models<-vector("list",length(clases)-1) #Preparamos una lista para los modelos entrenados en los dataset binarios
  clases<-sort(clases)
  for(x in 1:(length(clases)-1)){ #Se itera por todas las clases menos la última, para crear data set binarios y entrenar los modelos en los mismos
    
    if(x==1){
      indices<-which(df[,siz]==clases[x]) #En la primera iteración seleccionamos los indices de la primera clase
    }
    else{
      indices<-c(indices,which(df[,siz]==clases[x])) #En el resto de las iteraciones seleccionamos los indices anteriores y los de la clase actual
    }
    
    
    aux[[x]]<-df #Hacemos una copia del dataset completo en su posición en la lista de dataframes correspondiente a la iteración actual
    aux[[x]][,siz]<-as.integer(aux[[x]][,siz]) #Convertimos la variable a predecir en numérica para poder realizar operaciones de asignación aritméticas  
    aux[[x]][indices,siz]<-0 #Para el dataset binario actual, asigna los valores que se corresponden con la clase de la iteración actual el valor 0
    aux[[x]][,siz]<-ifelse(aux[[x]][,siz]==0,0,1) #Y asigna valor 1 para el resto de las clases
    aux[[x]][,siz]<-as.factor(aux[[x]][,siz]) #Se vuelve a definir la columna a predecir como factor para poder entrenar el modelo

    #Se define la fórmula a utilizar en el clasificador, en este caso será equivalente al 
    #nombre de la columna predictora  ~ ., es decir la variable a predecir en dependencia del resto
    #Es necesario definir la formula de antemano usando el nombre de la variable y no su posición o falla el clasificador
    
    formula<-as.formula(paste(colnames(aux[[x]])[siz],"~",".")) 
    models[[x]] <- do.call(classifier,list(formula,aux[[x]],...)) #Se entrena un modelo para el dataset binario actual a partir del clasificador y parámetros usados
  }
  return(models)
}

#Esta función recibe una lista de modelos que representan los dataset binarios del conjunto de datos original,
#Los elementos que se quieren predecir y las clases que representan los dataset binarios 
#(necesario para el caso de que el conjunto de test no tenga elementos de una clase definida en el entrenamiento)

#Se asume que los modelos son compatibles con la función predict(model,test,"probability"), como es el caso de los modelos de RWeka
#Devuelve un vector con la clasificación ordinal del elemento/s de test
predictUsingModels<-function(models,test,clases){
  predictedValues<-rep(NA,nrow(test)) # Preparamos un vector donde almacenar los valores predichos
  ClassProb<-vector("list",length(clases)) #Preparamos un vector donde almacenar las probabilidades de los dataset binarios por clase usando clasificación ordinal

  probabilities <- lapply(models,predict, test,"probability") #Usamos lapply para obtener una lista de probabilidades a partir de las predicciones de los modelos
  
  #models[[clase]][filas,prob0 o prob1]
  
  for(x in 1:length(clases)){ #Para cada clase
    if(x==1){ 
      ClassProb[[x]]<-1-probabilities[[x]][,2] #Pr(V1)=1-Pr(Target-V1)
    }
    if(x>1 & x<length(clases)){
      ClassProb[[x]]<-probabilities[[x-1]][,2]*(1-probabilities[[x]][,2]) #Pr(Vi)=Pr(Target>Vi-1)*(1-Pr(Target>Vi)), siendo i>1 y menor que el máximo
    }
    if(x==length(clases)){
      ClassProb[[x]]<-probabilities[[x-1]][,2] #Pr(Vk)=Pr(Target>Vk-1)
    }
  }
  
  #En este punto tenemos una lista de las probabilidades de clasificación ordinal por cada clase
  #el siguiente paso sería asignar al conjunto a predecir el valor con más probabilidad de los contenidos
  
  for(x in 1:nrow(test)){ #Por cada elemento a predecir
    maxProbability<-0 #Determina una variable de probabilidad
    for(y in 1:length(clases)){ #Entonces para cada clase:
      if(maxProbability<ClassProb[[y]][x]){ #Si la probabilidad de la clase actual que estamos analizando es mayor que la encontrada hasta el momento:
        maxProbability<-ClassProb[[y]][x] #Asigna esta probabilidad como la probabilidad máxima
        predictedValues[x]<-clases[[y]] #y asigna la clase al elemento actual
      }
    }
  }  
  return(predictedValues)
}

#Algunos elementos para probar
era<-read.arff("Material/Material/era.arff")
swd<-read.arff("Material/Material/swd.arff")
esl<-read.arff("Material/Material/esl.arff")
lev<-read.arff("Material/Material/lev.arff")

dts<-esl #selecciono el set de datos, por comodidad, para no cambiar variables abajo

#partición del dataset en 90% train, 10% test, balanceado
trainIndex <- createDataPartition(dts[,ncol(dts)], p = .9, list = FALSE, times = 1)
train <- dts[ trainIndex,]
test  <- dts[-trainIndex,]
clases<-as.integer(unique(dts[,ncol(dts)])) #separo las clases, en caso de que el test no tenga suficientes ejemplos

#ordeno el dataset de forma ascendiente para facilitar el proceso de clasificación ordinal
train<-train[order(train[,ncol(train)]),] 
test<-test[order(test[,ncol(test)]),]
clases<-sort(clases) #ordeno igualmente las clases de forma ascendiente para que coincida

models<-ordinalClassificatorModels(train,"JRip") # entrena modelo con dataset básico, nada de CV ni Grid-search

predictedValues<-predictUsingModels(models,test,clases) #predice sobre el test

print(paste("Accuracy: ", mean(predictedValues==test[,ncol(test)])))



"""
La generación de funciones flexibles para la clasificación ordinal fue desafiante, pero los resultados fueron decepcionantes.
La teoría está muy bien, y quizás con un conjunto de datos más grande se tendría una precisión mayor, pero con
la creación de dataset binarios los modelos de J48 y JRip, entre otros probados se generan modelos muy malos.
La capacidad del modelo para tratar dataset binarios es directamente proporcional a la calidad del modelo obtenido.
La clasificación ordinal también puede ser un poco confusa ya que las probabilidades deben ser normalizadas a 1 con tal de ser explicadas a terceros
puesto que la suma de las probabilidades de todas las clases es probable que sea superior a 1.
No obstante, en muchos casos es mejor que aplicar el clasificador directamente, lo cual indica que la separación para añadir la información de orden
puede llegar a ser de beneficio, en algunos casos con un aumento del 5% sobre el clasificador sin usar el modelo de separación para clasificación ordinal
"""