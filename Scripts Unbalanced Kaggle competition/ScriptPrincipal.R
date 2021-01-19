#Librerías necesarias
library(readr)
library(caret)
library(randomForest)
library(e1071)
library(kknn)
library(imbalance)
library(unbalanced)
library(ROSE)
library(smotefamily)
library(NoiseFiltersR)
library(xgboost)
library(ordinal)
library(ordinalForest)
library(nnet)
library(frbs)

seed<-156374562

#Carga de datos
dataset <- read_csv("Data/train.csv")
dataset <- as.data.frame(dataset)

#Eliminación de ID del conjunto de entrenamiento
dataset$id<-NULL

#Preprocesamiento opcional según tipo a probar
dataset$y<-as.factor(dataset$y)
dataset[,1:29]<-scale(dataset[,1:29])

sel <- c("X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")

dataset<-dataset[,sel]

#partición del dataset en 90% train, 10% test, balanceado
set.seed(seed) #Se define la semilla de aleatoriedad para asegurar consistencia en el sampling
trainIndex <- createDataPartition(dataset$y, p = .80, list = FALSE, times = 1)
trainor <- dataset[trainIndex,]
testor  <- dataset[-trainIndex,]
if(class(dataset$y)=="factor"){
clases<-(as.integer(unique(dataset$y))-1) #separo las clases, en caso de que el test no tenga suficientes ejemplos
}else{
clases<-unique(dataset$y)  
}


#ordeno el dataset de forma ascendiente para facilitar el proceso de clasificación ordinal
trainor<-trainor[order(trainor$y),] 
testor<-testor[order(testor$y),]
clases<-sort(clases) #ordeno igualmente las clases de forma ascendiente para que coincida

rfparams<-c("TRUE","1000", "12", "1") #use RF pred, ntree, mtry,nodesize
svmparams<-c("TRU","radial","2","0.2","0.05") #use SVM pred, kernel, degree, coef0, epsilon
knnparams<-c("TRU","11","1","1") #use Knn pred,k,distance,ykernel
xgparams<-c("TRU") #use xgboost for monotonic prediction
ordforparams<-c("TRU","1000","100","5000") #use ordinalforest package ordinalforest, nsets, ntreeperdiv, ntreefinal
clmparams<-c("TRU") #use CLM
nnetparams<-c("TRU") #use Neural Network, no config available
frbsparams<-c("TRU","WM") #use Fuzzy rule based system, method type ,config is special and optional as defined below

#fuzzy.control<-list(num.labels = 7, type.mf = "TRAPEZOID", type.tnorm = "MIN",type.defuz = "COG", type.implication.func = "ZADEH", name = "Sim-0") #change the control according to the fuzzy rule used

useOversample<-FALSE
useOVA<-TRUE
useOVO<-FALSE

hist(trainor$y)

#testS<-testSuite(trainor,testor,useOversample,useOVA,useOVO,rfparams,svmparams,knnparams,xgparams,ordforparams,clmparams,nnetparams,frbsparams,fuzzy.control)
testS<-testSuite(trainor,testor,useOversample,useOVA,useOVO,rfparams,svmparams,knnparams,xgparams,ordforparams,clmparams,nnetparams,frbsparams)
testS

set.seed(seed)
model<-randomForest::randomForest(y~.,trainor,ntree=1000, mtry=12, nodesize = 5)
predictedValues<-predict(model,newdata=testor)


OVADataset<-GenerarDatasetBinariosOVA(trainor)
set.seed(seed)
models<-EntrenarModelosOVA(OVADataset,preprocess = FALSE,"randomForest",ntree=2000, mtry=12, nodesize = 1)

predictedValues<-ProbabilityOVA(models,testor,clases)
predictedValues<-ProbabilityOVAw(models,testor,clases,3)
predictedValues<-OrdinalPredictUsingModels(models,testor,clases)


RMSE(round(predictedValues),testor$y)

predictedValues


####
#PARA LA PREDICCION FINAL A SUBIR
####

#Carga de datos de test
test <- read_csv("Data/test.csv")
test_id<-test$id #separa el id, no necesario para predicción, pero por claridad
test$id<-NULL #elimino el id ya separado
#test[,1:29]<-scale(test[,1:29])

predictedValues
predictedValues<-ProbabilityOVA(models,test,clases)
if(class(trainor$y)=="numeric"){
  predictedValues<-round(predictedValues)
}

#Preparación y escritura de los datos
df<-data.frame(test_id,predictedValues)
colnames(df)<-c("Id","Predicted")
write_csv(df,path="Probability OVA adj with importance variable selection RF.csv")
