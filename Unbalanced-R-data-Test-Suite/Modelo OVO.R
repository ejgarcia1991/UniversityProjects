GenerarDatasetBinariosOVO<-function(df){
  siz<-ncol(df)
  aux<-vector("list", 45)
  if(class(df$y)=="factor"){
    clases<-(as.integer(unique(df$y))-1)
  }else{
    clases<-unique(df$y)  
  }
  clases<-sort(clases)
  count<-0
  for(x in 1:(length(clases)-1)){ 
    for(y in (x+1):(length(clases))){
      count<-count+1
      indicesx<-which(df[,siz]==clases[x])
      indicesy<-which(df[,siz]==clases[y])
      aux[[count]]<- data.frame(rbind(df[indicesx, ], df[indicesy, ]))
      if(class(aux[[count]]$y)=="factor"){
        aux[[count]]$y<-as.factor(as.integer(aux[[count]]$y)-1)
      }
    }
  }
  return(aux)
}

EntrenarModelosOVO<-function(OVOdataset,preprocess,classifier,...){
  models<-vector("list",length(OVOdataset))
  for(x in 1:length(models)){
    datosProcesados<-OVOdataset[[x]]
    if(preprocess==TRUE){
    datosProcesados<-ProcesarDatosOVO(OVOdataset[[x]],x)
    }
    formula<-as.formula(paste(colnames(datosProcesados[ncol(datosProcesados)]),"~","."))
    set.seed(seed)
    models[[x]] <- do.call(classifier,list(formula,datosProcesados,...))
  }
  return(models)
}

OVOPredictUsingModelsRegression<-function(models,test,clases,notKNN=TRUE){
  predictedValues<-rep(NA,nrow(test))
  clases<-sort(clases)
  assignedClass <- vector("list", length(models))
  if(notKNN){
  assignedClass <- lapply(models,predict,test)}else{
  for(x in 1:(length(modelos))){
    assignedClass[[x]]<-models[[x]]$fitted.values
  }
  } #Usamos lapply para obtener una lista de probabilidades a partir de las predicciones de los modelos
  if(class(test$y)!="factor"){
  assignedClass <- lapply(assignedClass,round)
  }
  for(x in 1:nrow(test)){ #Por cada elemento a predecir
    votes<-rep(0,10)
    for(y in 1:length(assignedClass)){ #Y por cada modelo
      votes[1]<-votes[1]+ifelse(assignedClass[[y]][x]==0,1,0)
      votes[2]<-votes[2]+ifelse(assignedClass[[y]][x]==1,1,0)
      votes[3]<-votes[3]+ifelse(assignedClass[[y]][x]==2,1,0)
      votes[4]<-votes[4]+ifelse(assignedClass[[y]][x]==3,1,0)
      votes[5]<-votes[5]+ifelse(assignedClass[[y]][x]==4,1,0)
      votes[6]<-votes[6]+ifelse(assignedClass[[y]][x]==5,1,0)
      votes[7]<-votes[7]+ifelse(assignedClass[[y]][x]==6,1,0)
      votes[8]<-votes[8]+ifelse(assignedClass[[y]][x]==7,1,0)
      votes[9]<-votes[9]+ifelse(assignedClass[[y]][x]==8,1,0)
      votes[10]<-votes[10]+ifelse(assignedClass[[y]][x]==9,1,0)
    }
    pos<-0
    max<-0
    for(z in 1:10){
      if(votes[z]>max){
        max<-votes[z]
        pos<-z
      }
    }
    predictedValues[x]<-pos
  }
  return(predictedValues)
}

OVOPredictUsingModelsClassificationVotes<-function(models,test,clases,notKNN=TRUE){
  predictedValues<-rep(NA,nrow(test))
  clases<-sort(clases)
  assignedClass <- vector("list", length(models))
  if(notKNN){
    assignedClass <- lapply(models,predict,test)}else{
      for(x in 1:(length(modelos))){
        assignedClass[[x]]<-models[[x]]$fitted.values
      }
    }
  for(x in 1:nrow(test)){
    votes<-rep(0,10)
    for(y in 1:length(assignedClass)){
      votes[1]<-votes[1]+ifelse(assignedClass[[y]][x]==0,1,0)
      votes[2]<-votes[2]+ifelse(assignedClass[[y]][x]==1,1,0)
      votes[3]<-votes[3]+ifelse(assignedClass[[y]][x]==2,1,0)
      votes[4]<-votes[4]+ifelse(assignedClass[[y]][x]==3,1,0)
      votes[5]<-votes[5]+ifelse(assignedClass[[y]][x]==4,1,0)
      votes[6]<-votes[6]+ifelse(assignedClass[[y]][x]==5,1,0)
      votes[7]<-votes[7]+ifelse(assignedClass[[y]][x]==6,1,0)
      votes[8]<-votes[8]+ifelse(assignedClass[[y]][x]==7,1,0)
      votes[9]<-votes[9]+ifelse(assignedClass[[y]][x]==8,1,0)
      votes[10]<-votes[10]+ifelse(assignedClass[[y]][x]==9,1,0)
    }
    pos<-0
    max<-0
    for(z in 1:10){
      if(votes[z]>max){
        max<-votes[z]
        pos<-z
      }
    }
    predictedValues[x]<-pos
  }
  return(predictedValues)
}

OVOPredictUsingModelsClassificationProb<-function(models,test,clases){
  predictedValues<-rep(NA,nrow(test))
  clases<-sort(clases)
  assignedClass <- lapply(models,predict,test,"prob") #Usamos lapply para obtener una lista de probabilidades a partir de las predicciones de los modelos

  for(x in 1:nrow(test)){ #Por cada elemento a predecir
    prob<-rep(0,10)
    for(y in 1:length(assignedClass)){ #Y por cada modelo
      prob[1]<-prob[1]+ifelse(colnames(assignedClass[[y]])[1]=="0",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="0",assignedClass[[y]][x,2],0))
      prob[2]<-prob[2]+ifelse(colnames(assignedClass[[y]])[1]=="1",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="1",assignedClass[[y]][x,2],0))
      prob[3]<-prob[3]+ifelse(colnames(assignedClass[[y]])[1]=="2",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="2",assignedClass[[y]][x,2],0))
      prob[4]<-prob[4]+ifelse(colnames(assignedClass[[y]])[1]=="3",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="3",assignedClass[[y]][x,2],0))
      prob[5]<-prob[5]+ifelse(colnames(assignedClass[[y]])[1]=="4",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="4",assignedClass[[y]][x,2],0))
      prob[6]<-prob[6]+ifelse(colnames(assignedClass[[y]])[1]=="5",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="5",assignedClass[[y]][x,2],0))
      prob[7]<-prob[7]+ifelse(colnames(assignedClass[[y]])[1]=="6",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="6",assignedClass[[y]][x,2],0))
      prob[8]<-prob[8]+ifelse(colnames(assignedClass[[y]])[1]=="7",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="7",assignedClass[[y]][x,2],0))
      prob[9]<-prob[9]+ifelse(colnames(assignedClass[[y]])[1]=="8",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="8",assignedClass[[y]][x,2],0))
      prob[10]<-prob[10]+ifelse(colnames(assignedClass[[y]])[1]=="9",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="9",assignedClass[[y]][x,2],0))
    }
    pos<-0
    max<-0
    for(z in 1:10){
      if(prob[z]>max){
        max<-prob[z]
        pos<-z
      }
    }
    predictedValues[x]<-pos
  }
  return(predictedValues)
}

SVMOVOPredictUsingModelsClassificationProb<-function(models,test,clases){
  predictedValues<-rep(NA,nrow(test))
  clases<-sort(clases)
  assignedClass <- lapply(models,predict,test,probability=TRUE) #Usamos lapply para obtener una lista de probabilidades a partir de las predicciones de los modelos
  
  for(x in 1:length(assignedClass)){
    assignedClass[[x]]<-attr(assignedClass[[x]],"probabilities")
  }
  
  for(x in 1:nrow(test)){ #Por cada elemento a predecir
    prob<-rep(0,10)
    for(y in 1:length(assignedClass)){ #Y por cada modelo
      prob[1]<-prob[1]+ifelse(colnames(assignedClass[[y]])[1]=="0",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="0",assignedClass[[y]][x,2],0))
      prob[2]<-prob[2]+ifelse(colnames(assignedClass[[y]])[1]=="1",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="1",assignedClass[[y]][x,2],0))
      prob[3]<-prob[3]+ifelse(colnames(assignedClass[[y]])[1]=="2",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="2",assignedClass[[y]][x,2],0))
      prob[4]<-prob[4]+ifelse(colnames(assignedClass[[y]])[1]=="3",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="3",assignedClass[[y]][x,2],0))
      prob[5]<-prob[5]+ifelse(colnames(assignedClass[[y]])[1]=="4",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="4",assignedClass[[y]][x,2],0))
      prob[6]<-prob[6]+ifelse(colnames(assignedClass[[y]])[1]=="5",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="5",assignedClass[[y]][x,2],0))
      prob[7]<-prob[7]+ifelse(colnames(assignedClass[[y]])[1]=="6",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="6",assignedClass[[y]][x,2],0))
      prob[8]<-prob[8]+ifelse(colnames(assignedClass[[y]])[1]=="7",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="7",assignedClass[[y]][x,2],0))
      prob[9]<-prob[9]+ifelse(colnames(assignedClass[[y]])[1]=="8",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="8",assignedClass[[y]][x,2],0))
      prob[10]<-prob[10]+ifelse(colnames(assignedClass[[y]])[1]=="9",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="9",assignedClass[[y]][x,2],0))
    }
    pos<-0
    max<-0
    for(z in 1:10){
      if(prob[z]>max){
        max<-prob[z]
        pos<-z
      }
    }
    predictedValues[x]<-pos
  }
  return(predictedValues)
}

KNNOVOPredictUsingModelsClassificationProb<-function(models,test,clases){
  predictedValues<-rep(NA,nrow(test))
  clases<-sort(clases)

  for(x in 1:length(models)){
    assignedClass[[x]]<-models[[x]]$prob
  }
  
  for(x in 1:nrow(test)){ #Por cada elemento a predecir
    prob<-rep(0,10)
    for(y in 1:length(assignedClass)){ #Y por cada modelo
      prob[1]<-prob[1]+ifelse(colnames(assignedClass[[y]])[1]=="0",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="0",assignedClass[[y]][x,2],0))
      prob[2]<-prob[2]+ifelse(colnames(assignedClass[[y]])[1]=="1",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="1",assignedClass[[y]][x,2],0))
      prob[3]<-prob[3]+ifelse(colnames(assignedClass[[y]])[1]=="2",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="2",assignedClass[[y]][x,2],0))
      prob[4]<-prob[4]+ifelse(colnames(assignedClass[[y]])[1]=="3",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="3",assignedClass[[y]][x,2],0))
      prob[5]<-prob[5]+ifelse(colnames(assignedClass[[y]])[1]=="4",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="4",assignedClass[[y]][x,2],0))
      prob[6]<-prob[6]+ifelse(colnames(assignedClass[[y]])[1]=="5",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="5",assignedClass[[y]][x,2],0))
      prob[7]<-prob[7]+ifelse(colnames(assignedClass[[y]])[1]=="6",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="6",assignedClass[[y]][x,2],0))
      prob[8]<-prob[8]+ifelse(colnames(assignedClass[[y]])[1]=="7",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="7",assignedClass[[y]][x,2],0))
      prob[9]<-prob[9]+ifelse(colnames(assignedClass[[y]])[1]=="8",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="8",assignedClass[[y]][x,2],0))
      prob[10]<-prob[10]+ifelse(colnames(assignedClass[[y]])[1]=="9",assignedClass[[y]][x,1],ifelse(colnames(assignedClass[[y]])[2]=="9",assignedClass[[y]][x,2],0))
    }
    pos<-0
    max<-0
    for(z in 1:10){
      if(prob[z]>max){
        max<-prob[z]
        pos<-z
      }
    }
    predictedValues[x]<-pos
  }
  return(predictedValues)
}
