GenerarDatasetBinariosOVA<-function(df){
  siz<-ncol(df) #Se almacena en una variable la posición de la última variable por comodidad
  if(class(df$y)=="factor"){
    clases<-(as.integer(unique(df$y))-1)
  }else{
    clases<-unique(df$y)
  }
  clases<-sort(clases)
  aux<-vector("list", length(clases)-1) #Creamos una lista con tamaño equivalente al número de clases -1 para los data frames binarios
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
    aux[[x]][,siz]<-as.factor(aux[[x]][,siz])
  }
  return(aux)
} #Genera los 9 dataset binarios OVA

EntrenarModelosOVA<-function(OVAdataset,preprocess,classifier,...){
  models<-vector("list",length(OVAdataset))
  for(x in 1:length(models)){
    datosProcesados<-OVAdataset[[x]]
    if(preprocess==TRUE){
    datosProcesados<-ProcesarDatosOVA(OVAdataset[[x]],x)
    }
    formula<-as.formula(paste(colnames(datosProcesados[ncol(datosProcesados)]),"~","."))
    set.seed(seed)
    models[[x]] <- do.call(classifier,list(formula,datosProcesados,...))
  }
  return(models)
}

OrdinalPredictUsingModels<-function(models,test,clases){
  predictedValues<-rep(NA,nrow(test)) # Preparamos un vector donde almacenar los valores predichos
  ClassProb<-vector("list",length(clases)) #Preparamos un vector donde almacenar las probabilidades de los dataset binarios por clase usando clasificación ordinal
  probabilities <- lapply(models,predict, test,"prob") #Usamos lapply para obtener una lista de probabilidades a partir de las predicciones de los modelos
  
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
} #OVA ordinal usando predict de tipo probability

SVMOrdinalPredict<-function(models,test,clases){
  predictedValues<-rep(NA,nrow(test)) # Preparamos un vector donde almacenar los valores predichos
  ClassProb<-vector("list",length(clases)) #Preparamos un vector donde almacenar las probabilidades de los dataset binarios por clase usando clasificación ordinal
  probabilities <- lapply(models,predict, test,probability=TRUE) #Usamos lapply para obtener una lista de probabilidades a partir de las predicciones de los modelos
  
  for(x in 1:length(probabilities)){
    probabilities[[x]]<-attr(probabilities[[x]],"probabilities")
    if(colnames(probabilities[[x]])[1]=="1"){
      probabilities[[x]]<-probabilities[[x]][,2:1]
    }
  }
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
} #Usa un formato diferente para obtener las probabilidades

KNNOrdinalPredict<-function(models,test,clases){
  predictedValues<-rep(NA,nrow(test)) # Preparamos un vector donde almacenar los valores predichos
  ClassProb<-vector("list",length(clases)) #Preparamos un vector donde almacenar las probabilidades de los dataset binarios por clase usando clasificación ordinal
  for(x in 1:length(clases)){ #Para cada clase
    if(x==1){ 
      ClassProb[[x]]<-1-models[[x]]$prob[,2] #Pr(V1)=1-Pr(Target-V1)
    }
    if(x>1 & x<length(clases)){
      ClassProb[[x]]<-models[[x-1]]$prob[,2]*(1-models[[x]]$prob[,2]) #Pr(Vi)=Pr(Target>Vi-1)*(1-Pr(Target>Vi)), siendo i>1 y menor que el máximo
    }
    if(x==length(clases)){
      ClassProb[[x]]<-models[[x-1]]$prob[,2] #Pr(Vk)=Pr(Target>Vk-1)
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
} #Usa un formato diferente para obtener las probabilidades

OVAPredictUsingModels<-function(models,test,clases){
  predictedValues<-rep(1,nrow(test)) # Preparamos un vector donde almacenar los valores predichos, incluyendo el valor 1 de la formula
  clases<-sort(clases)
  
  assignedClass <- lapply(models,predict,test) #Usamos lapply para obtener una lista de probabilidades a partir de las predicciones de los modelos
  for(x in 1:nrow(test)){ #Por cada elemento a predecir
    for(y in 1:length(assignedClass)){ #Y por cada clase
      predictedValues[x]<-predictedValues[x]+(as.integer(assignedClass[[y]][x])-1)
    }
    predictedValues[x]<-clases[predictedValues[x]]
  }
  
  
  return(predictedValues)
} #OVA con votos

KNNOVAPredict<-function(models,test,clases){
  predictedValues<-rep(1,nrow(test)) # Preparamos un vector donde almacenar los valores predichos, incluyendo el valor 1 de la formula
  clases<-sort(clases)
  
  assignedClass <- models #Usamos lapply para obtener una lista de probabilidades a partir de las predicciones de los modelos
  
  for(x in 1:nrow(test)){ #Por cada elemento a predecir
    for(y in 1:length(assignedClass)){ #Y por cada clase
      predictedValues[x]<-predictedValues[x]+(as.integer(assignedClass[[y]]$fitted.values[x])-1)
    }
    predictedValues[x]<-clases[predictedValues[x]]
  }
  
  
  return(predictedValues)
}

ProbabilityOVA<-function(models,test,clases){
  predictedValues<-rep(NA,nrow(test)) # Preparamos un vector donde almacenar los valores predichos
  ClassProb<-vector("list",length(clases)) #Preparamos un vector donde almacenar las probabilidades de los dataset binarios por clase usando clasificación ordinal
  probabilities <- lapply(models,predict, test,"prob") #Usamos lapply para obtener una lista de probabilidades a partir de las predicciones de los modelos
  
  ClassProb<-vector("list",length(clases))
  for(x in 1:length(clases)){
    ClassProb[[x]]<-rep(1,nrow(test))
  }
  for(z in 1:nrow(test)){
    maxProbability<-0
    for(x in 1:length(clases)){ #Para cada clase
      for(y in 1:(length(clases)-1)){
        prob<-ifelse(x<=y,probabilities[[y]][z,1],probabilities[[y]][z,2])
        if(prob==0){
          prob<-0.001
        }
        ClassProb[[x]][z]<-ClassProb[[x]][z]*prob
      }
      if(maxProbability<ClassProb[[x]][z]){ #Si la probabilidad de la clase actual que estamos analizando es mayor que la encontrada hasta el momento:
        maxProbability<-ClassProb[[x]][z] #Asigna esta probabilidad como la probabilidad máxima
        predictedValues[z]<-clases[[x]] #y asigna la clase al elemento actual
      }
    }
    
  }
  
  return(predictedValues)
}

ProbabilityOVAw<-function(models,test,clases, factor=1){
  predictedValues<-rep(NA,nrow(test)) # Preparamos un vector donde almacenar los valores predichos
  ClassProb<-vector("list",length(clases)) #Preparamos un vector donde almacenar las probabilidades de los dataset binarios por clase usando clasificación ordinal
  probabilities <- lapply(models,predict, test,"prob") #Usamos lapply para obtener una lista de probabilidades a partir de las predicciones de los modelos
  
  ClassProb<-vector("list",length(clases))
  alpha<-1/((length(clases)-1)*factor)
  for(x in 1:length(clases)){
    ClassProb[[x]]<-rep(1,nrow(test))
  }
  for(z in 1:nrow(test)){
    maxProbability<-0
    for(x in 1:length(clases)){ #Para cada clase
      for(y in 1:(length(clases)-1)){
        dist<-abs(x-y)
        if(dist==0){
          dist<-1
        }
        prob<-ifelse(x<=y,probabilities[[y]][z,1],probabilities[[y]][z,2])
        if(prob==0){
          prob<-0.001
        }
        ClassProb[[x]][z]<-ClassProb[[x]][z]*prob
        ClassProb[[x]][z]<-ClassProb[[x]][z]*(1-(dist*alpha))
      }
      if(maxProbability<ClassProb[[x]][z]){ #Si la probabilidad de la clase actual que estamos analizando es mayor que la encontrada hasta el momento:
        maxProbability<-ClassProb[[x]][z] #Asigna esta probabilidad como la probabilidad máxima
        predictedValues[z]<-clases[[x]] #y asigna la clase al elemento actual
      }
    }
    
  }
  
  return(predictedValues)
}
