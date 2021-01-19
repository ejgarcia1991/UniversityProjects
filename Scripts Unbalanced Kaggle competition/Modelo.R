RMSE<-function(predictedValues,realValues){
  x<-as.integer(predictedValues)-1
  y<-as.integer(realValues)-1
  
  sqrt(mean(Mod(x-y)*Mod(x-y)))
}

GenerarDatasetBinariosOneClass<-function(df){
  siz<-ncol(df) #Se almacena en una variable la posición de la última variable por comodidad
  clases<-(as.integer(unique(df[,ncol(df)])))-1 #Se almacena un vector conteniendo las clases de la variable a predecir
  aux<-vector("list", length(clases)) #Creamos una lista con tamaño equivalente al número de clases -1 para los data frames binarios
  clases<-sort(clases)
  for(x in 1:(length(clases))){
    #Se itera por todas las clases menos la última, para crear data set binarios y entrenar los modelos en los mismos
    indices<-which(df[,siz]==clases[x]) #En la primera iteración seleccionamos los indices de la primera clase
    
    aux[[x]]<-df #Hacemos una copia del dataset completo en su posición en la lista de dataframes correspondiente a la iteración actual
    aux[[x]][,siz]<-as.integer(aux[[x]][,siz]) #Convertimos la variable a predecir en numérica para poder realizar operaciones de asignación aritméticas  
    aux[[x]][indices,siz]<-1 #Para el dataset binario actual, asigna los valores que se corresponden con la clase de la iteración actual el valor 0
    aux[[x]][,siz]<-ifelse(aux[[x]][,siz]==1,1,0) #Y asigna valor 1 para el resto de las clases
    aux[[x]][,siz]<-as.factor(aux[[x]][,siz]) #Se vuelve a definir la columna a predecir como factor para poder entrenar el modelo
  }
  return(aux)
}

OversampleData<-function(dataset){
  oversampledDataset<-dataset
  binariosOneClass<-GenerarDatasetBinariosOneClass(oversampledDataset)
  siz<-ncol(oversampledDataset)
  #------------------
  #Clase 0, 22 casos
  #------------------
  train<-binariosOneClass[[1]]
  
  
  set.seed(seed)
  synth<-imbalance::mwmote(train,numInstances=120,kNoisy = 5,kMajority =3 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
  synth<-imbalance::neater(train,synth,k=3,iterations=200,classAttr = "y")
  synth$y<-as.integer(synth$y)-1
  synth[,siz]<-0
  synth$y<-as.factor(synth$y)
  
  oversampledDataset<-rbind(oversampledDataset,synth)
  
  #------------------
  #Clase 1, 141 casos
  #------------------
  train<-binariosOneClass[[2]]
  
  set.seed(seed)
  synth<-imbalance::mwmote(train,numInstances=175,kNoisy = 5,kMajority =3 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
  synth<-imbalance::neater(train,synth,k=3,iterations=200,classAttr = "y")
  synth$y<-as.integer(synth$y)-1
  synth[,siz]<-1
  synth$y<-as.factor(synth$y)
  
  oversampledDataset<-rbind(oversampledDataset,synth)
  
  #------------------
  #Clase 7, 89 casos
  #------------------
  train<-binariosOneClass[[8]]
  
  set.seed(seed)
  synth<-imbalance::mwmote(train,numInstances=110,kNoisy = 5,kMajority =3 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
  synth<-imbalance::neater(train,synth,k=3,iterations=200,classAttr = "y")
  synth$y<-as.integer(synth$y)-1
  synth[,siz]<-7
  synth$y<-as.factor(synth$y)
  
  oversampledDataset<-rbind(oversampledDataset,synth)
  
  #------------------
  #Clase 8, 46 casos
  #------------------
  train<-binariosOneClass[[9]]
  
  set.seed(seed)
  synth<-imbalance::mwmote(train,numInstances=105,kNoisy = 5,kMajority =3 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
  synth<-imbalance::neater(train,synth,k=3,iterations=200,classAttr = "y")
  synth$y<-as.integer(synth$y)-1
  synth[,siz]<-8
  synth$y<-as.factor(synth$y)
  
  oversampledDataset<-rbind(oversampledDataset,synth)
  
  #------------------
  #Clase 9, 22 casos
  #------------------
  train<-binariosOneClass[[9]]
  
  set.seed(seed)
  synth<-imbalance::mwmote(train,numInstances=95,kNoisy = 5,kMajority =3 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
  synth<-imbalance::neater(train,synth,k=3,iterations=200,classAttr = "y")
  
  synth$y<-as.integer(synth$y)-1
  synth[,siz]<-9
  synth$y<-as.factor(synth$y)
  
  oversampledDataset<-rbind(oversampledDataset,synth)
  
  return(oversampledDataset)  
}

testSuite<-function(train,test,useOversample,useOVA,useOVO,rfparams,svmparams,knnparams,xgparams,ordforparams,clmparams,nnetparams,frbsparams,fuzzy.control=NULL){
  trainor<-train
  testor<-test
  if(useOVA){
  OVADataset<-GenerarDatasetBinariosOVA(trainor)
  }
  if(useOVO){
  OVODataset<-GenerarDatasetBinariosOVO(trainor)
  }
  if(useOversample){
  trainor$y<-as.factor(trainor$y)
  Oversampledtrain<-OversampleData(trainor)
  if(class(train$y)=="numeric"){
    Oversampledtrain$y<-as.numeric(Oversampledtrain$y)-1
    trainor$y<-as.numeric(trainor$y)-1
  }
  }
  df<-data.frame(cbind("Sample","3.33"))
  df[,1]<-as.character(df[,1])
  df[,2]<-as.character(df[,2])
  colnames(df)<-c("Algorithm","RMSE")
  
if(rfparams[1]=="TRUE"){
#------------------
#NORMAL RF
#------------------
set.seed(seed)
model<-randomForest::randomForest(y~.,trainor,ntree=as.integer(rfparams[2]), mtry=as.integer(rfparams[3]), nodesize = as.integer(rfparams[4]))
predictedValues<-predict(model,newdata=testor)
if(class(train$y)=="numeric"){
predictedValues<-round(predictedValues)
}
df<-rbind(df,c("Normal Random Forest", RMSE(predictedValues,testor$y)))
if(useOversample){
#------------------
#Oversampled NORMAL RF
#------------------
set.seed(seed)
model<-randomForest::randomForest(y~.,Oversampledtrain,ntree=as.integer(rfparams[2]), mtry=as.integer(rfparams[3]), nodesize = as.integer(rfparams[4]))
predictedValues<-predict(model,newdata=testor)
if(class(train$y)=="numeric"){
predictedValues<-round(predictedValues)
}
df<-rbind(df,c("Oversampled Normal Random Forest", RMSE(predictedValues,testor$y)))
}
if(useOVA){
#------------------
#Ordinal OVA RF
#------------------
set.seed(seed)
models<-EntrenarModelosOVA(OVADataset,preprocess = FALSE,"randomForest",ntree=as.integer(rfparams[2]), mtry=as.integer(rfparams[3]), nodesize = as.integer(rfparams[4]))
predictedValues<-OrdinalPredictUsingModels(models,testor,clases)
df<-rbind(df,c("Ordinal OVA Random Forest", RMSE(predictedValues,testor$y)))

#------------------
#Processed Ordinal OVA RF
#------------------
set.seed(seed)
models<-EntrenarModelosOVA(OVADataset,preprocess = TRUE,"randomForest",ntree=as.integer(rfparams[2]), mtry=as.integer(rfparams[3]), nodesize = as.integer(rfparams[4]))
predictedValues<-OrdinalPredictUsingModels(models,testor,clases)
df<-rbind(df,c("Processed Ordinal OVA Random Forest", RMSE(predictedValues,testor$y)))

#------------------
#Additive OVA RF
#------------------
set.seed(seed)
models<-EntrenarModelosOVA(OVADataset,preprocess = FALSE,"randomForest",ntree=as.integer(rfparams[2]), mtry=as.integer(rfparams[3]), nodesize = as.integer(rfparams[4]))
predictedValues<-OVAPredictUsingModels(models,testor,clases)
df<-rbind(df,c("Additive OVA Random Forest", RMSE(predictedValues,testor$y)))

#------------------
#Processed Additive OVA RF
#------------------
set.seed(seed)
models<-EntrenarModelosOVA(OVADataset,preprocess=TRUE,"randomForest",ntree=as.integer(rfparams[2]), mtry=as.integer(rfparams[3]), nodesize = as.integer(rfparams[4]))
predictedValues<-OVAPredictUsingModels(models,testor,clases)
df<-rbind(df,c("Processed Additive OVA Random Forest", RMSE(predictedValues,testor$y)))
}
if(useOVO){
#------------------
#OVO REGRESSION RF
#------------------
if(class(train$y)=="numeric"){
  set.seed(seed)
  models<-EntrenarModelosOVO(OVODataset,preprocess = FALSE,"randomForest",ntree=as.integer(rfparams[2]), mtry=as.integer(rfparams[3]), nodesize = as.integer(rfparams[4]))
  predictedValues<-OVOPredictUsingModelsRegression(models,testor,clases)
  df<-rbind(df,c("OVO regression Random Forest", RMSE(predictedValues,testor$y)))
}

#------------------
#OVO Clasificacion RF Votos
#------------------
if(class(train$y)=="factor"){
  set.seed(seed)
  models<-EntrenarModelosOVO(OVODataset,preprocess = FALSE,"randomForest",ntree=as.integer(rfparams[2]), mtry=as.integer(rfparams[3]), nodesize = as.integer(rfparams[4]))
  predictedValues<-OVOPredictUsingModelsClassificationVotes(models,testor,clases)
  df<-rbind(df,c("OVO vote based classification Random Forest", RMSE(predictedValues,testor$y)))
}

#------------------
#OVO Clasificacion RF prob
#------------------
if(class(train$y)=="factor"){
  set.seed(seed)
  models<-EntrenarModelosOVO(OVODataset,preprocess = FALSE,"randomForest",ntree=as.integer(rfparams[2]), mtry=as.integer(rfparams[3]), nodesize = as.integer(rfparams[4]))
  predictedValues<-OVOPredictUsingModelsClassificationProb(models,testor,clases)
  df<-rbind(df,c("OVO probability based classification Random Forest", RMSE(predictedValues,testor$y)))
}

#------------------
#OVO REGRESSION RF
#------------------
if(class(train$y)=="numeric"){
  set.seed(seed)
  models<-EntrenarModelosOVO(OVODataset,preprocess = FALSE,"randomForest",ntree=as.integer(rfparams[2]), mtry=as.integer(rfparams[3]), nodesize = as.integer(rfparams[4]))
  predictedValues<-OVOPredictUsingModelsRegression(models,testor,clases)
  df<-rbind(df,c("OVO regression Random Forest", RMSE(predictedValues,testor$y)))
}

#------------------
#OVO Clasificacion RF Votos
#------------------
if(class(train$y)=="factor"){
  set.seed(seed)
  models<-EntrenarModelosOVO(OVODataset,preprocess = FALSE,"randomForest",ntree=as.integer(rfparams[2]), mtry=as.integer(rfparams[3]), nodesize = as.integer(rfparams[4]))
  predictedValues<-OVOPredictUsingModelsClassificationVotes(models,testor,clases)
  df<-rbind(df,c("OVO vote based classification Random Forest", RMSE(predictedValues,testor$y)))
}

#------------------
#OVO Clasificacion RF prob
#------------------
if(class(train$y)=="factor"){
  set.seed(seed)
  models<-EntrenarModelosOVO(OVODataset,preprocess = FALSE,"randomForest",ntree=as.integer(rfparams[2]), mtry=as.integer(rfparams[3]), nodesize = as.integer(rfparams[4]))
  predictedValues<-OVOPredictUsingModelsClassificationProb(models,testor,clases)
  df<-rbind(df,c("OVO probability based classification Random Forest", RMSE(predictedValues,testor$y)))
}

#------------------
#Processed OVO REGRESSION RF
#------------------
if(class(train$y)=="numeric"){
  set.seed(seed)
  models<-EntrenarModelosOVO(OVODataset,preprocess = TRUE,"randomForest",ntree=as.integer(rfparams[2]), mtry=as.integer(rfparams[3]), nodesize = as.integer(rfparams[4]))
  predictedValues<-OVOPredictUsingModelsRegression(models,testor,clases)
  df<-rbind(df,c("OVO regression Random Forest", RMSE(predictedValues,testor$y)))
}

#------------------
#Processed OVO Clasificacion RF Votos
#------------------
if(class(train$y)=="factor"){
  set.seed(seed)
  models<-EntrenarModelosOVO(OVODataset,preprocess = TRUE,"randomForest",ntree=as.integer(rfparams[2]), mtry=as.integer(rfparams[3]), nodesize = as.integer(rfparams[4]))
  predictedValues<-OVOPredictUsingModelsClassificationVotes(models,testor,clases)
  df<-rbind(df,c("Processed OVO vote based classification Random Forest", RMSE(predictedValues,testor$y)))
}

#------------------
#Processed OVO Clasificacion RF prob
#------------------
if(class(train$y)=="factor"){
  set.seed(seed)
  models<-EntrenarModelosOVO(OVODataset,preprocess = TRUE,"randomForest",ntree=as.integer(rfparams[2]), mtry=as.integer(rfparams[3]), nodesize = as.integer(rfparams[4]))
  predictedValues<-OVOPredictUsingModelsClassificationProb(models,testor,clases)
  df<-rbind(df,c("Processed OVO probability based classification Random Forest", RMSE(predictedValues,testor$y)))
}
}
}
if(svmparams[1]=="TRUE"){
#------------------
#NORMAL SVM
#------------------
set.seed(seed)
model<-svm(y~.,trainor,kernel=svmparams[2],degree=as.integer(svmparams[3]),coef0=as.numeric(svmparams[4]),epsilon=as.numeric(svmparams[5]),probability=TRUE)
predictedValues<-predict(model,testor)
if(class(train$y)=="numeric"){
  predictedValues<-round(predictedValues)
}
df<-rbind(df,c("Normal SVM", RMSE(predictedValues,testor$y)))
if(useOversample){
#------------------
#Oversampled NORMAL SVM
#------------------
set.seed(seed)
model<-svm(y~.,Oversampledtrain,kernel=svmparams[2],degree=as.integer(svmparams[3]),coef0=as.numeric(svmparams[4]),epsilon=as.numeric(svmparams[5]),probability=TRUE)
predictedValues<-predict(model,testor)
if(class(train$y)=="numeric"){
predictedValues<-round(predictedValues)
}
df<-rbind(df,c("Oversampled Normal SVM", RMSE(predictedValues,testor$y)))
}
if(useOVA){
#------------------
#Ordinal OVA SVM
#------------------
set.seed(seed)
models<-EntrenarModelosOVA(OVADataset,preprocess = FALSE,"svm",kernel=svmparams[2],degree=as.integer(svmparams[3]),coef0=as.numeric(svmparams[4]),epsilon=as.numeric(svmparams[5]),probability=TRUE)
predictedValues<-SVMOrdinalPredict(models,testor,clases)
df<-rbind(df,c("Ordinal OVA SVM", RMSE(predictedValues,testor$y)))

#------------------
#Processed Ordinal OVA SVM
#------------------
set.seed(seed)
models<-EntrenarModelosOVA(OVADataset,preprocess = TRUE,"svm",kernel=svmparams[2],degree=as.integer(svmparams[3]),coef0=as.numeric(svmparams[4]),epsilon=as.numeric(svmparams[5]),probability=TRUE)
predictedValues<-SVMOrdinalPredict(models,testor,clases)
df<-rbind(df,c("Processed Ordinal OVA SVM", RMSE(predictedValues,testor$y)))

#------------------
#Additive OVA SVM
#------------------
set.seed(seed)
models<-EntrenarModelosOVA(OVADataset,preprocess = FALSE,"svm",kernel=svmparams[2],degree=as.integer(svmparams[3]),coef0=as.numeric(svmparams[4]),epsilon=as.numeric(svmparams[5]),probability=TRUE)
predictedValues<-OVAPredictUsingModels(models,testor,clases)
df<-rbind(df,c("Additive OVA SVM", RMSE(predictedValues,testor$y)))

#------------------
#Processed Additive OVA SVM
#------------------
set.seed(seed)
models<-EntrenarModelosOVA(OVADataset,preprocess = TRUE,"svm",kernel=svmparams[2],degree=as.integer(svmparams[3]),coef0=as.numeric(svmparams[4]),epsilon=as.numeric(svmparams[5]),probability=TRUE)
predictedValues<-OVAPredictUsingModels(models,testor,clases)
df<-rbind(df,c("Processed Additive OVA SVM", RMSE(predictedValues,testor$y)))
}
if(useOVO){
#------------------
#OVO REGRESSION SVM
#------------------
if(class(train$y)=="numeric"){
  set.seed(seed)
  models<-EntrenarModelosOVO(OVODataset,preprocess = FALSE,"svm",kernel=svmparams[2],degree=as.integer(svmparams[3]),coef0=as.numeric(svmparams[4]),epsilon=as.numeric(svmparams[5]),probability=TRUE)
  predictedValues<-OVOPredictUsingModelsRegression(models,testor,clases)
  df<-rbind(df,c("OVO regression SVM", RMSE(predictedValues,testor$y)))
}

#------------------
#OVO Clasificacion SVM votes
#------------------
if(class(train$y)=="factor"){
  set.seed(seed)
  models<-EntrenarModelosOVO(OVODataset,preprocess = FALSE,"svm",kernel=svmparams[2],degree=as.integer(svmparams[3]),coef0=as.numeric(svmparams[4]),epsilon=as.numeric(svmparams[5]),probability=TRUE)
  predictedValues<-OVOPredictUsingModelsClassificationVotes(models,testor,clases)
  df<-rbind(df,c("OVO vote based classification SVM", RMSE(predictedValues,testor$y)))
}

#------------------
#OVO Clasificacion SVM prob
#------------------
if(class(train$y)=="factor"){
  set.seed(seed)
  models<-EntrenarModelosOVO(OVODataset,preprocess = FALSE,"svm",kernel=svmparams[2],degree=as.integer(svmparams[3]),coef0=as.numeric(svmparams[4]),epsilon=as.numeric(svmparams[5]),probability=TRUE)
  predictedValues<-SVMOVOPredictUsingModelsClassificationProb(models,testor,clases)
  df<-rbind(df,c("OVO probability based classification SVM", RMSE(predictedValues,testor$y)))
}

#------------------
#Processed OVO REGRESSION SVM
#------------------
if(class(train$y)=="numeric"){
  set.seed(seed)
  models<-EntrenarModelosOVO(OVODataset,preprocess = TRUE,"svm",kernel=svmparams[2],degree=as.integer(svmparams[3]),coef0=as.numeric(svmparams[4]),epsilon=as.numeric(svmparams[5]),probability=TRUE)
  predictedValues<-OVOPredictUsingModelsRegression(models,testor,clases)
  df<-rbind(df,c("Processed OVO regression SVM", RMSE(predictedValues,testor$y)))
}

#------------------
#Processed OVO Clasificacion SVM votes
#------------------
if(class(train$y)=="factor"){
  set.seed(seed)
  models<-EntrenarModelosOVO(OVODataset,preprocess = TRUE,"svm",kernel=svmparams[2],degree=as.integer(svmparams[3]),coef0=as.numeric(svmparams[4]),epsilon=as.numeric(svmparams[5]),probability=TRUE)
  predictedValues<-OVOPredictUsingModelsClassificationVotes(models,testor,clases)
  df<-rbind(df,c("Processed OVO vote based classification SVM", RMSE(predictedValues,testor$y)))
}

#------------------
#Processed OVO Clasificacion SVM prob
#------------------
if(class(train$y)=="factor"){
  set.seed(seed)
  models<-EntrenarModelosOVO(OVODataset,preprocess = TRUE,"svm",kernel=svmparams[2],degree=as.integer(svmparams[3]),coef0=as.numeric(svmparams[4]),epsilon=as.numeric(svmparams[5]),probability=TRUE)
  predictedValues<-SVMOVOPredictUsingModelsClassificationProb(models,testor,clases)
  df<-rbind(df,c("Processed OVO probability based classification SVM", RMSE(predictedValues,testor$y)))
}
}

}
if(knnparams[1]=="TRUE"){
#------------------
#NORMAL KNN
#------------------
set.seed(seed)
model<-kknn(y~.,trainor,testor,k=as.integer(knnparams[2]),distance=as.integer(knnparams[3]),ykernel=as.integer(knnparams[4]))
predictedValues<-model$fitted.values
if(class(train$y)=="numeric"){
predictedValues<-round(predictedValues)
}
df<-rbind(df,c("Normal KNN", RMSE(predictedValues,testor$y)))
if(useOversample){
#------------------
#Oversampled NORMAL KNN
#------------------
set.seed(seed)
model<-kknn(y~.,Oversampledtrain,testor,k=as.integer(knnparams[2]),distance=as.integer(knnparams[3]),ykernel=as.integer(knnparams[4]))
predictedValues<-model$fitted.values
if(class(train$y)=="numeric"){
predictedValues<-round(predictedValues)
}
df<-rbind(df,c("Oversampled Normal KNN", RMSE(predictedValues,testor$y)))
}
if(useOVA){
#------------------
#Ordinal OVA KNN
#------------------
set.seed(seed)
models<-EntrenarModelosOVA(OVADataset,preprocess = FALSE,"kknn",testor,k=as.integer(knnparams[2]),distance=as.integer(knnparams[3]),ykernel=as.integer(knnparams[4]))
predictedValues<-KNNOrdinalPredict(models,testor,clases)
df<-rbind(df,c("Ordinal OVA KNN", RMSE(predictedValues,testor$y)))

#------------------
#Processed Ordinal OVA KNN
#------------------
set.seed(seed)
models<-EntrenarModelosOVA(OVADataset,preprocess = TRUE,"kknn",testor,k=as.integer(knnparams[2]),distance=as.integer(knnparams[3]),ykernel=as.integer(knnparams[4]))
predictedValues<-KNNOrdinalPredict(models,testor,clases)
df<-rbind(df,c("Processed Ordinal OVA KNN", RMSE(predictedValues,testor$y)))

#------------------
#Additive OVA KNN
#------------------
set.seed(seed)
models<-EntrenarModelosOVA(OVADataset,preprocess = FALSE,"kknn",testor,k=as.integer(knnparams[2]),distance=as.integer(knnparams[3]),ykernel=as.integer(knnparams[4]))
predictedValues<-KNNOVAPredict(models,testor,clases)
df<-rbind(df,c("Additive OVA KNN", RMSE(predictedValues,testor$y)))

#------------------
#Processed Additive OVA Oversampled NORMAL KNN
#------------------
set.seed(seed)
models<-EntrenarModelosOVA(OVADataset,preprocess = TRUE,"kknn",testor,k=as.integer(knnparams[2]),distance=as.integer(knnparams[3]),ykernel=as.integer(knnparams[4]))
predictedValues<-KNNOVAPredict(models,testor,clases)
df<-rbind(df,c("Processed Additive OVA KNN", RMSE(predictedValues,testor$y)))
}
if(useOVO){

#------------------
#OVO REGRESSION KNN
#------------------
if(class(train$y)=="numeric"){
  set.seed(seed)
  models<-EntrenarModelosOVO(OVODataset,preprocess = FALSE,"kknn",testor,k=as.integer(knnparams[2]),distance=as.integer(knnparams[3]),ykernel=as.integer(knnparams[4]))
  predictedValues<-OVOPredictUsingModelsRegression(models,testor,clases,FALSE)
  df<-rbind(df,c("OVO regression KNN", RMSE(predictedValues,testor$y)))
}

#------------------
#OVO Clasificacion KNN votes
#------------------
if(class(train$y)=="factor"){
  set.seed(seed)
  models<-EntrenarModelosOVO(OVODataset,preprocess = FALSE,"kknn",testor,k=as.integer(knnparams[2]),distance=as.integer(knnparams[3]),ykernel=as.integer(knnparams[4]))
  predictedValues<-OVOPredictUsingModelsClassificationVotes(models,testor,clases,FALSE)
  df<-rbind(df,c("OVO vote based classification KNN", RMSE(predictedValues,testor$y)))
}

#------------------
#OVO Clasificacion KNN prob
#------------------
if(class(train$y)=="factor"){
  set.seed(seed)
  models<-EntrenarModelosOVO(OVODataset,preprocess = FALSE,"kknn",testor,k=as.integer(knnparams[2]),distance=as.integer(knnparams[3]),ykernel=as.integer(knnparams[4]))
  predictedValues<-KNNOVOPredictUsingModelsClassificationProb(models,testor,clases)
  df<-rbind(df,c("OVO probability based classification KNN", RMSE(predictedValues,testor$y)))
}



#------------------
#Processed OVO REGRESSION KNN
#------------------
if(class(train$y)=="numeric"){
  set.seed(seed)
  models<-EntrenarModelosOVO(OVODataset,preprocess = TRUE,"kknn",testor,k=as.integer(knnparams[2]),distance=as.integer(knnparams[3]),ykernel=as.integer(knnparams[4]))
  predictedValues<-OVOPredictUsingModelsRegression(models,testor,clases,FALSE)
  df<-rbind(df,c("Processed OVO regression KNN", RMSE(predictedValues,testor$y)))
}

#------------------
#Processed OVO Clasificacion KNN votes
#------------------
if(class(train$y)=="factor"){
  set.seed(seed)
  models<-EntrenarModelosOVO(OVODataset,preprocess = TRUE,"kknn",testor,k=as.integer(knnparams[2]),distance=as.integer(knnparams[3]),ykernel=as.integer(knnparams[4]))
  predictedValues<-OVOPredictUsingModelsClassificationVotes(models,testor,clases,FALSE)
  df<-rbind(df,c("Processed OVO vote based classification KNN", RMSE(predictedValues,testor$y)))
}

#------------------
#Processed OVO Clasificacion KNN prob
#------------------
if(class(train$y)=="factor"){
  set.seed(seed)
  models<-EntrenarModelosOVO(OVODataset,preprocess = TRUE,"kknn",testor,k=as.integer(knnparams[2]),distance=as.integer(knnparams[3]),ykernel=as.integer(knnparams[4]))
  predictedValues<-KNNOVOPredictUsingModelsClassificationProb(models,testor,clases)
  df<-rbind(df,c("Processed OVO probability based classification KNN", RMSE(predictedValues,testor$y)))
}
}
}
if(xgparams[1]=="TRUE"){
#------------------
#MONOTONIC classification
#------------------
if(class(train$y)=="factor"){
set.seed(seed)
models<-MonotonicClassificatorModelTraining(trainor)
predictedValues<-monotonicPredictUsingModels(models,testor,clases)
df<-rbind(df,c("Monotonic XGBoost", RMSE(predictedValues,testor$y)))
}
  
#------------------
#Oversampled MONOTONIC classification
#------------------
  if(useOversample){
if(class(train$y)=="factor"){
set.seed(seed)
models<-MonotonicClassificatorModelTraining(Oversampledtrain)
predictedValues<-monotonicPredictUsingModels(models,testor,clases)
df<-rbind(df,c("Oversampled Monotonic XGBoost", RMSE(predictedValues,testor$y)))
}
  }
}
#------------------
#Ordinal forest package
#------------------
if(ordforparams[1]=="TRUE"){
if(class(train$y)=="factor"){

  set.seed(seed)
  model <- ordfor(depvar="y", data=trainor, nsets=as.integer(ordforparams[2]), ntreeperdiv=as.integer(ordforparams[3]),ntreefinal=as.integer(ordforparams[4]), perffunction = "proportional")
  predictedValues<-predict(ordforres,newdata=testor)
  df<-rbind(df,c("Ordinal Forest", RMSE(predictedValues$ypred,testor$y)))
    
#------------------
#Oversampled Ordinal forest package
#------------------
if(useOversample){
  set.seed(seed)
  model <- ordfor(depvar="y", data=Oversampledtrain, nsets=as.integer(ordforparams[2]), ntreeperdiv=as.integer(ordforparams[3]),ntreefinal=as.integer(ordforparams[4]), perffunction = "proportional")
  predictedValues<-predict(ordforres,newdata=testor)
  df<-rbind(df,c("Oversampled Ordinal Forest", RMSE(predictedValues$ypred,testor$y)))
}
}
}
#------------------
#cummulative link models
#------------------
if(clmparams[1]=="TRUE"){
if(class(train$y)=="factor"){

    set.seed(seed)
    model <- clm(y ~ ., data=trainor)
    predictedValues<-predict(model,testor,type="class")
    df<-rbind(df,c("CLM", RMSE(predictedValues$fit,testor$y)))
    
#------------------
#Oversampled cummulative link models
#------------------
if(useOversample){
    set.seed(seed)
    model <- clm(y ~ ., data=Oversampledtrain)
    predictedValues<-predict(model,testor,type="class")
    df<-rbind(df,c("Oversampled CLM", RMSE(predictedValues$fit,testor$y)))
    }
  }
}
#------------------
#Single Layer Neural Network
#------------------
if(nnetparams[1]=="TRUE"){
if(class(train$y)=="numeric"){
model<-multinom(y~.,trainor)
predictedValues<-predict(model,testor,type="class")
df<-rbind(df,c("NNet", RMSE(predictedValues,testor$y)))
}
}
  
#------------------
#Fuzzy Rules Based System
#------------------
  if(frbsparams[1]=="TRUE"){
    if(class(trainor$y)=="factor"){
      trainor$y<-as.numeric(trainor$y)
    }else{
      trainor$y<-trainor$y+1
    }
  range.data.input <- apply(trainor, 2, range)
  
  if(is.null(fuzzy.control)){
    model <- frbs.learn(trainor,range.data.input,method.type = frbsparams[2])
  }else{
    model <- frbs.learn(trainor,range.data.input,method.type = frbsparams[2],fuzzy.control)
  }
  predictedValues<-round(predict(model,testor[,-ncol(testor)]))-1
  df<-rbind(df,c("Fuzzy Rules Based System", RMSE(predictedValues,testor$y)))
  }
  
return(df)
}
