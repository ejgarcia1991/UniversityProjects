#Se cargan la librería necesaria para el clasificador usado, en los ejemplos se usa RWeka
library(xgboost)
library(RWeka) #necesario para los read.arff
library(caret) #particiones de datos

#Esta función recibe un dataframe sobre el que crear dataset binarios y entrenar modelo monotónico
#se asume que la última columna es la clase a predecir
#Ejemplo de uso 1: modelos<- ordinalClassficatorModelos(train,"JRip")

MonotonicClassificatorModelTraining<-function(df){
  #-----------------
  #Preparación
  #-----------------
  
  siz<-ncol(df) #Se almacena en una variable la posición de la última variable por comodidad
  clases<-as.integer(unique(df[,ncol(df)])) #Se almacena un vector conteniendo las clases de la variable a predecir
  aux<-vector("list", length(clases)-1) #Creamos una lista con tamaño equivalente al número de clases -1 para los data frames binarios
  models<-vector("list",length(clases)-1) #Preparamos una lista para los modelos entrenados en los dataset binarios
  clases<-sort(clases) #Se reordenan las clases para que sea más fácil mantener la restricción de monotonía
  
  #-----------------
  #Dataset binarios y entrenamiento de modelos en los mismos
  #-----------------
  for(x in 2:(length(clases))){ #Se itera por todas las clases, para crear data set binarios y entrenar los modelos en los mismos, 
                                  #debido a la forma de crear los dataset binarios conservando monotonía, se comienza en 2, ya que la primera clase el clasificador solo asignaría 1
                                  #y esto se añade en la formula predictora final
    if(x==2){
      indices<-which(df[,siz]==clases[x-1]) #En la primera iteración seleccionamos los indices de la primera clase
    }
    else{
      indices<-c(indices,which(df[,siz]==clases[x-1])) #En el resto de las iteraciones seleccionamos los indices anteriores y los de la clase anterior a la actual, para obtener los que son estrictamente menores
    }
    
    aux[[x-1]]<-df #Hacemos una copia del dataset completo en su posición en la lista de dataframes correspondiente a la iteración actual
    aux[[x-1]][,siz]<-as.integer(aux[[x-1]][,siz]) #Convertimos la variable a predecir en numérica para poder realizar operaciones de asignación aritméticas, este paso es opcional si la clase no está definida como factor 
    aux[[x-1]][indices,siz]<-0 #Para el dataset binario actual, asigna los valores que se corresponden con la clase de la iteración actual el valor 0, representando valores menores estricto que el valor actual
    aux[[x-1]][,siz]<-ifelse(aux[[x-1]][,siz]==0,0,1) #Y asigna valor 1 para el resto de las clases, representado clases mayor o igual que la actual
    
    #Notar que en esta asignación, la monotonía se mantiene debido a la ordenación de clases previamente, 
    #este paso es fundamental, de lo contrario sería necesario hacer comparaciones entre clases por cada asignación
    
    #Entrenamiento de modelo con el dataset binario
    
    negative_cases <- sum(aux[[x-1]][,siz] == 0)
    postive_cases <- sum(aux[[x-1]][,siz] == 1)
    
    xgbMatrixTrain<-xgb.DMatrix(as.matrix(aux[[x-1]][,1:(siz-1)]),label=aux[[x-1]][,siz]) #Se prepara el dataset a entrenar por XGBoost
    models[[x-1]] <- xgboost(data =xgbMatrixTrain,
                             params = list(monotone_constraints=1,tree_method="exact"),
                             max.depth = 5,
                             nrounds=15,
#                             early_stopping_rounds = 3,
#                             objective="binary:logistic",
#                             scale_pos_weight = negative_cases/postive_cases
                            )
  }
 return(models)
}

#Esta función recibe una lista de modelos que representan los dataset binarios del conjunto de datos original,
#Los elementos que se quieren predecir y las clases que representan los dataset binarios 
#(necesario para el caso de que el conjunto de test no tenga elementos de una clase definida en el entrenamiento)

#Se asume que los modelos son compatibles con la función predict(model,test) y que son modelos entrenados sobre dataSet binarios con un rango entre 0 y 1
#Devuelve un vector con la clasificación del elemento/s de test usando restricciones monotonicas
monotonicPredictUsingModels<-function(models,test,clases){
  
  predictedValues<-rep(1,nrow(test)) # Preparamos un vector donde almacenar los valores predichos, incluyendo el valor 1 de la formula
  xgbMatrixTest<-xgb.DMatrix(as.matrix(test[,1:(ncol(test)-1)]),label=test[,ncol(test)]) #Preparación de DMatrix para XGboost
  clases<-sort(clases)
  
  probabilities <- lapply(models,predict, xgbMatrixTest) #Usamos lapply para obtener una lista de probabilidades a partir de las predicciones de los modelos
  assignedClass <- lapply(probabilities,round) #para la clasificación monotónica binaria se trabaja con 0 y 1 así que redondeo
  
  
  for(x in 1:nrow(test)){ #Por cada elemento a predecir
    for(y in 1:length(assignedClass)){ #Y por cada clase
      predictedValues[x]<-predictedValues[x]+assignedClass[[y]][x]
    }
    predictedValues[x]<-clases[predictedValues[x]]
  }


  return(predictedValues)
}

#Algunos elementos para probar
era<-read.arff("Material/Material/era.arff")
swd<-read.arff("Material/Material/swd.arff")
esl<-read.arff("Material/Material/esl.arff")
lev<-read.arff("Material/Material/lev.arff")


dts<-swd #selecciono el set de datos, por comodidad, para no cambiar variables abajo


#partición del dataset en 90% train, 10% test, balanceado
set.seed(1372501) #Se define la semilla de aleatoriedad para asegurar consistencia en los elementos aleatorios
trainIndex <- createDataPartition(dts[,ncol(dts)], p = .9, list = FALSE, times = 1)
train <- dts[ trainIndex,]
test  <- dts[-trainIndex,]
clases<-as.integer(unique(dts[,ncol(dts)])) #separo las clases, en caso de que el test no tenga suficientes ejemplos

#ordeno el dataset de forma ascendiente para facilitar el proceso de debugging, no influye en el resultado final
train<-train[order(train[,ncol(train)]),]
test<-test[order(test[,ncol(test)]),]
clases<-sort(clases) #ordeno igualmente las clases de forma ascendiente para que coincida

modelos<-MonotonicClassificatorModelTraining(train) # entrena modelo con dataset básico, nada de CV ni Grid-search

pred<-monotonicPredictUsingModels(modelos,test,clases) #predice valores en el conjunto de test

print(paste("Accuracy: ", mean(pred==test[,ncol(test)])))




"""
Fue muy interesante, pero realmente muy complejo la comprensión de la lógica detrás de la clasificación monotónica
Me tomó varios días llegar a comprender realmente lo que significa y como un problema multinomial podría clasificarse
usando técnicas monotónicas que solo están previstas para clases binarias, como el XGBoost.
No entendí casi nada de lo que decía el laboratorio, así que tuve que buscar documentos y artículos científicos
puesto que la clasificación monotónica multinomial es un campo muy reciente y no hay documentación robusta sobre la misma
Una vez comprendida la lógica detrás del proceso la programación de las funciones fue relativamente sencillo.
Comparando la clasificación monotónica con la ordinal sin la restricción de monotonía se observa un aumento de la precisión.
Es decir, en problemas donde esta condición exista, el aplicar la restricción de monotonía puede influenciar positivamente el modelo.
La separación OVA del modelo y el proceso de clasificación posterior indica que el error obtenido no va a ser superior al de los modelos individuales.
Esto hace que sea factible utilizar técnicas monotónicas como XGBoost en problemas de clasificación multinomial, aunque requiera de un elevado número de pasos de preprocesamiento.
"""

