



## OVO 


multicol<-function(data){
  R<-cor(data, method="spearman")
  l<-eigen(R)$values
  IC<-max(l)/min(l)
  FIV<-diag(solve(R))
  p<-ncol(R)
  Heo<-1-p/sum(1/l)
  
  return(list(IC=IC, FIVs=FIV, indiceHeo=Heo))
}


library(caret)
library(Hmisc)
library(pROC)

### cargamos los datos originales para fusionar algunas variables a los imputados
setwd("C:/Users/MiguelNmerino/Google Drive/DATCOM/PRADO/ASIGNATURAS/M3-AVANZADA/COMPETICION/datos")

# cargar datos
train <- read.csv("train.csv", 
                  header = TRUE, sep=",", 
                  na.strings=c(".","NA","","?", "unknown", "Unknown", "-"))
TEST <-  read.csv("test.csv", sep =",", header = TRUE)



#VALIDACION OVO
VALIDACION <- train
CVIndices <- createFolds(VALIDACION$y, k=2)  ## k=2 50-50 
VALIDACION <- VALIDACION[CVIndices[[1]],]
VALIDACION_X <- VALIDACION[,1:29]

VALIDACION_Y <- VALIDACION$y



#########################################################################################################

####################   #  #   #####################
#                datos clase 0 vs 1               #
###################################################

datos_A <- train[which(train$y == 0),]
datos_B <- train[which(train$y == 1),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(0:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_22", "X_2", "X_15", "X_16", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:8]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=25, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predicciones_val <- c()
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predicciones <- c()
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##










#########################################################################################################

####################   #  #   #####################
#                datos clase 0 vs 2               #
###################################################

datos_A <- train[which(train$y == 0),]
datos_B <- train[which(train$y == 2),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_15", "X_2", "X_22", "X_5", "X_11", "X_4", "X_16", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:13]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

table(dataTrain$y)

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.6

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 2, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=40, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma

predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##





















#########################################################################################################

####################   #  #   #####################
#                datos clase 0 vs 3               #
###################################################

datos_A <- train[which(train$y == 0),]
datos_B <- train[which(train$y == 3),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(2:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_2","X_15","X_6","X_5","X_11","X_4","X_23", "X_17", "X_16", "X_21", "X_20", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:7]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

table(dataTrain$y)

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.7

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =3 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=20, mtry=3, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma

predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##




















#########################################################################################################

####################   #  #   #####################
#                datos clase 0 vs 4               #
###################################################

datos_A <- train[which(train$y == 0),]
datos_B <- train[which(train$y == 4),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_11","X_15","X_2","X_23","X_6","X_5", "X_20", "X_21", "X_0", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:7]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

table(dataTrain$y)

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.4

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=25, mtry=5, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma

predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##




















#########################################################################################################

####################   #  #   #####################
#                datos clase 0 vs 5               #
###################################################

datos_A <- train[which(train$y == 0),]
datos_B <- train[which(train$y == 5),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_2","X_5","X_6","X_15","X_21","X_20","X_16","X_17","X_23", "X_0", "X_10", "X_9", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:7]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

table(dataTrain$y)

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.8

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=50, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma

predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##




















#########################################################################################################

####################   #  #   #####################
#                datos clase 0 vs 6               #
###################################################

datos_A <- train[which(train$y == 0),]
datos_B <- train[which(train$y == 6),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0","X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_5","X_2","X_6","X_15","X_11","X_16","X_20","X_4","X_17","X_10", "X_9","y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:7]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

table(dataTrain$y)

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.4

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=25, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma

predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##




















#########################################################################################################

####################   #  #   #####################
#                datos clase 0 vs 7               #
###################################################

datos_A <- train[which(train$y == 0),]
datos_B <- train[which(train$y == 7),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(2:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_20","X_6","X_5","X_15","X_4","X_2","X_11","X_17", "X_16", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:6]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

table(dataTrain$y)

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=25, mtry=5, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma

predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##




















#########################################################################################################

####################   #  #   #####################
#                datos clase 0 vs 8               #
###################################################

datos_A <- train[which(train$y == 0),]
datos_B <- train[which(train$y == 8),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(2:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_20","X_6","X_4","X_5","X_10","X_15","X_2","X_11", "X_23", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:3]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

table(dataTrain$y)

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=40, mtry=3, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma

predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##




















#########################################################################################################

####################   #  #   #####################
#                datos clase 0 vs 9               #
###################################################

datos_A <- train[which(train$y == 0),]
datos_B <- train[which(train$y == 9),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(2:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_10", "X_2", "X_6", "X_4", "X_15", "X_20", "X_5", "X_23", "X_17",  "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:3]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

table(dataTrain$y)

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=25, mtry=5, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma

predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##







####################################################################################################
####################################################################################################













#########################################################################################################

####################   #  #   #####################
#                datos clase 1 vs 2               #
###################################################

datos_A <- train[which(train$y == 1),]
datos_B <- train[which(train$y == 2),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_4", "X_2", "X_5", "X_23", "X_26", "X_15", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:8]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=50, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##










#########################################################################################################

####################   #  #   #####################
#                datos clase 1 vs 3               #
###################################################

datos_A <- train[which(train$y == 1),]
datos_B <- train[which(train$y == 3),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_18", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_2","X_23","X_4","X_5","X_20","X_6","X_22","X_21","X_10", "X_11","X_17", "X_16", "X_15",  "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:6]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

table(dataTrain$y)

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 2, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=40, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma

predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##










#########################################################################################################

####################   #  #   #####################
#                datos clase 1 vs 4               #
###################################################

datos_A <- train[which(train$y == 1),]
datos_B <- train[which(train$y == 4),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_18", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_18", "X_20", "X_21", "X_22", "X_23", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:5]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

table(dataTrain$y)

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.04

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 2, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=40, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma

predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##










#########################################################################################################

####################   #  #   #####################
#                datos clase 1 vs 5               #
###################################################

datos_A <- train[which(train$y == 1),]
datos_B <- train[which(train$y == 5),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_18", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_18", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:3]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

table(dataTrain$y)

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.03

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 2, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=50, mtry=5, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma

predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##










#########################################################################################################

####################   #  #   #####################
#                datos clase 1 vs 6               #
###################################################

datos_A <- train[which(train$y == 1),]
datos_B <- train[which(train$y == 6),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_18", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_18", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:5]

## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

table(dataTrain$y)

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 2, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=40, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma

predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##










#########################################################################################################

####################   #  #   #####################
#                datos clase 1 vs 7               #
###################################################

datos_A <- train[which(train$y == 1),]
datos_B <- train[which(train$y == 7),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_18", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_2", "X_4", "X_5", "X_6", "X_10", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:4]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

table(dataTrain$y)

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 2, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=40, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma

predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##










#########################################################################################################

####################   #  #   #####################
#                datos clase 1 vs 8               #
###################################################

datos_A <- train[which(train$y == 1),]
datos_B <- train[which(train$y == 8),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_18", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_2", "X_4",  "X_6", "X_10", "X_15", 
         "X_16", "X_18", "X_20", "X_21", "X_22", "X_23", "X_26", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:5]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

table(dataTrain$y)

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 2, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=40, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma

predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##










#########################################################################################################

####################   #  #   #####################
#                datos clase 1 vs 9               #
###################################################

datos_A <- train[which(train$y == 1),]
datos_B <- train[which(train$y == 9),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_18", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_2", "X_4", "X_6", "X_10", "X_11", "X_15", 
         "X_16",  "X_18", "X_21", "X_22", "X_23", "X_26", "X_27", "X_20", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:4]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

table(dataTrain$y)

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 2, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=40, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma

predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##










#########################################################################################################

####################   #  #   #####################
#                datos clase 2 vs 3               #
###################################################

datos_A <- train[which(train$y == 2),]
datos_B <- train[which(train$y == 3),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_20","X_23","X_22","X_2","X_11","X_21", "X_0","X_4","X_15","X_6",  "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:3]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=100, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##












#########################################################################################################

####################   #  #   #####################
#                datos clase 2 vs 4               #
###################################################

datos_A <- train[which(train$y == 2),]
datos_B <- train[which(train$y == 4),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0","X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_0","X_2", "X_4", "X_5", "X_6", "X_10", "X_11", "X_15", 
         "X_20", "X_21", "X_22", "X_23", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:8]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=100, mtry=3, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##











#########################################################################################################

####################   #  #   #####################
#                datos clase 2 vs 5               #
###################################################

datos_A <- train[which(train$y == 2),]
datos_B <- train[which(train$y == 5),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:5]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=50, mtry=4, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##











#########################################################################################################

####################   #  #   #####################
#                datos clase 2 vs 6               #
###################################################

datos_A <- train[which(train$y == 2),]
datos_B <- train[which(train$y == 6),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0","X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_0","X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:5]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=45, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##











#########################################################################################################

####################   #  #   #####################
#                datos clase 2 vs 7               #
###################################################

datos_A <- train[which(train$y == 2),]
datos_B <- train[which(train$y == 7),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_2", "X_4",  "X_6", "X_9", "X_10", "X_11", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:3]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=35, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##











#########################################################################################################

####################   #  #   #####################
#                datos clase 2 vs 8               #
###################################################

datos_A <- train[which(train$y == 2),]
datos_B <- train[which(train$y == 8),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_2", "X_4", "X_6", "X_10", "X_11", "X_15", 
          "X_20", "X_21", "X_22", "X_23", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:7]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=35, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##











#########################################################################################################

####################   #  #   #####################
#                datos clase 2 vs 9               #
###################################################

datos_A <- train[which(train$y == 2),]
datos_B <- train[which(train$y == 9),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_10", "X_2","X_6","X_17","X_15","X_21","X_22", "X_20", "X_26", "X_23", "X_5", "X_4", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:5]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=50, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##











#########################################################################################################

####################   #  #   #####################
#                datos clase 3 vs 4               #
###################################################

datos_A <- train[which(train$y == 3),]
datos_B <- train[which(train$y == 4),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_4", "X_10", "X_21", "X_2", "X_26", "X_5","X_0",  "X_20","y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:9]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=100, mtry=5, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##












#########################################################################################################

####################   #  #   #####################
#                datos clase 3 vs 5               #
###################################################

datos_A <- train[which(train$y == 3),]
datos_B <- train[which(train$y == 5),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11",
         "X_16", "X_17", "X_20", "X_21", "X_26", "X_27", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:2]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=100, mtry=4, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##












#########################################################################################################

####################   #  #   #####################
#                datos clase 3 vs 6               #
###################################################

datos_A <- train[which(train$y == 3),]
datos_B <- train[which(train$y == 6),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:4]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=100, mtry=5, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##












#########################################################################################################

####################   #  #   #####################
#                datos clase 3 vs 7               #
###################################################

datos_A <- train[which(train$y == 3),]
datos_B <- train[which(train$y == 7),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_10", "X_15", 
          "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:6]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=50, mtry=3, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##












#########################################################################################################

####################   #  #   #####################
#                datos clase 3 vs 8               #
###################################################

datos_A <- train[which(train$y == 3),]
datos_B <- train[which(train$y == 8),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_20","X_2","X_21","X_10","X_22","X_4","X_6", "X_23", "X_15", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:6]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=100, mtry=5, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##












#########################################################################################################

####################   #  #   #####################
#                datos clase 3 vs 9               #
###################################################

datos_A <- train[which(train$y == 3),]
datos_B <- train[which(train$y == 9),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_0", "X_2", "X_4", "X_6", "X_10", "X_11", "X_15", 
         "X_20", "X_21", "X_22", "X_23", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:4]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=50, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##













#########################################################################################################

####################   #  #   #####################
#                datos clase 4 vs 5               #
###################################################

datos_A <- train[which(train$y == 4),]
datos_B <- train[which(train$y == 5),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_10","X_0", "X_4", "X_15","X_6", "X_21","X_9", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:5]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=100, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##












#########################################################################################################

####################   #  #   #####################
#                datos clase 4 vs 6               #
###################################################

datos_A <- train[which(train$y == 4),]
datos_B <- train[which(train$y == 6),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_16","X_28","X_4","X_10","X_20","X_27","X_5","X_2","X_15", "X_0", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:2]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=150, mtry=5, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##












#########################################################################################################

####################   #  #   #####################
#                datos clase 4 vs 5               #
###################################################

datos_A <- train[which(train$y == 4),]
datos_B <- train[which(train$y == 7),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_10", "X_11", "X_15", 
         "X_20", "X_21", "X_22", "X_23", "X_26",  "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:2]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=100, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##












#########################################################################################################

####################   #  #   #####################
#                datos clase 4 vs 8               #
###################################################

datos_A <- train[which(train$y == 4),]
datos_B <- train[which(train$y == 8),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_20","X_10","X_2","X_6","X_21","X_22","X_4","X_15","X_23", "X_11", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:7]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=100, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##












#########################################################################################################

####################   #  #   #####################
#                datos clase 4 vs 9               #
###################################################

datos_A <- train[which(train$y == 4),]
datos_B <- train[which(train$y == 9),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_2","X_10","X_26","X_6", "X_23", "X_4", "X_22", "X_21",  "X_0", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:5]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.5

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=100, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##











#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################






#########################################################################################################

####################   #  #   #####################
#                datos clase 5 vs 6               #
###################################################

datos_A <- train[which(train$y == 5),]
datos_B <- train[which(train$y == 6),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_27", "X_2", "X_26", "X_10", "X_11", "X_6", "X_5",  "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:8]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=150, mtry=5, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##












#########################################################################################################

####################   #  #   #####################
#                datos clase 5 vs 7               #
###################################################

datos_A <- train[which(train$y == 5),]
datos_B <- train[which(train$y == 7),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10","X_15", 
         "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_15", "X_2", "X_21", "X_6", "X_22", "X_26", "X_23", "X_4", "X_0",   "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:6]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=50, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##












#########################################################################################################

####################   #  #   #####################
#                datos clase 5 vs 8               #
###################################################

datos_A <- train[which(train$y == 5),]
datos_B <- train[which(train$y == 8),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_15", "X_2", "X_20", "X_21", "X_22", "X_23", "X_6", "X_4","X_10",   "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:5]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=100, mtry=3, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##












#########################################################################################################

####################   #  #   #####################
#                datos clase 5 vs 9               #
###################################################

datos_A <- train[which(train$y == 5),]
datos_B <- train[which(train$y == 9),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_10", "X_21", "X_20", "X_2", "X_0", "X_4", "X_23", "X_6", "X_15", "X_22",  "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:7]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=100, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##













#########################################################################################################

####################   #  #   #####################
#                datos clase 6 vs 7               #
###################################################

datos_A <- train[which(train$y == 6),]
datos_B <- train[which(train$y == 7),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_2","X_6", "X_4", "X_20","X_22",  "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:4]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=60, mtry=3, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##









#########################################################################################################

####################   #  #   #####################
#                datos clase 6 vs 8               #
###################################################

datos_A <- train[which(train$y == 6),]
datos_B <- train[which(train$y == 8),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_20","X_22", "X_11","X_21", "X_2", "X_15", "X_6", "X_26", "X_4",  "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:5]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=50, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##









#########################################################################################################

####################   #  #   #####################
#                datos clase 6 vs 9              #
###################################################

datos_A <- train[which(train$y == 6),]
datos_B <- train[which(train$y == 9),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_10","X_2", "X_22","X_23", "X_21", "X_6", "X_0", "X_11", "X_4", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:6]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]
table(dataTrain$y)
###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.5

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 5, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=100, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##








##########################################################################




##########################################################################



#########################################################################################################

####################   #  #   #####################
#                datos clase 7 vs 8               #
###################################################

datos_A <- train[which(train$y == 7),]
datos_B <- train[which(train$y == 8),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_20","X_26", "X_0", "X_2", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:7]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=30, mtry=2, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##










####################   #  #   #####################
#                datos clase 7 vs 9               #
###################################################

datos_A <- train[which(train$y == 7),]
datos_B <- train[which(train$y == 9),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_10","X_20", "X_22", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:5]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=40, mtry=3, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##






######################################################################################################

######################################################################################################





####################   #  #   #####################
#                datos clase 8 vs 9               #
###################################################

datos_A <- train[which(train$y == 8),]
datos_B <- train[which(train$y == 9),]
datosAB <- rbind(datos_A, datos_B)

# particiones
set.seed(46)
CVIndices <- createFolds(datosAB$y, k=15)  ## k=2 50-50 
#First partition
dataTrain <- datosAB[-CVIndices[[1]],]
dataTest  <- datosAB[CVIndices[[1]],]
dim(dataTrain)
dim(dataTest)


# multicolinealidad
multicol(datosAB[ ,c(1:30)])
names(datosAB)

#seleccion de variables
sel <- c("X_0", "X_2", "X_4", "X_5", "X_6", "X_9", "X_10", "X_11", "X_15", 
         "X_16", "X_17", "X_20", "X_21", "X_22", "X_23", "X_26", "X_27", "X_28", "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]
multicol(dataTrain[,1:ncol(dataTrain)-1])

# seleccion de caracteristicas
#library(Boruta)
set.seed(46)
bor.train <- Boruta(as.factor(y)~.,data=dataTrain,doTrace=2)
bor.train
par(mfrow=c(1,1))
plot(bor.train)

#seleccion de variables  ## REAJUSTAR
sel <- c("X_2","X_10", "X_23", "X_15","X_0",  "y")
dataTrain <- dataTrain[ ,sel]
dataTest <- dataTest[, sel]



## outliers
#resultados <- mvoutlier::uni.plot(dataTrain[,1:9])
#dataTrain <- dataTrain[!resultados$outliers,]
#table(dataTrain$y)



##DISTANCE BASED OUTLIERS (LOF)
library(DMwR)
lof.scores <- lofactor(dataTrain, k = 5)
indices.segun.lof.score.ordenados <- order(lof.scores, decreasing = TRUE)
lof.scores.ordenados <- lof.scores[indices.segun.lof.score.ordenados]
lof.scores.ordenados
par(mfrow=c(1,1))
plot(lof.scores.ordenados)

outliers <- indices.segun.lof.score.ordenados[1:6]


## ruido
#library(NoiseFiltersR)
#set.seed(1)
#out <- NoiseFiltersR::IPF(as.factor(y)~., data=dataTrain)
#summary(out, explicit=TRUE)
#out$cleanData

dataTrain <- dataTrain[-outliers ,sel]
dataTest <- dataTest[, sel]

###
n.insta = abs(table(dataTrain$y)[1]-table(dataTrain$y)[2])*1.1

set.seed(46)
library(smotefamily)
synth<-imbalance::mwmote(dataTrain, numInstances=n.insta, kNoisy = 3, kMajority =5 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(dataTrain, synth, k=3, iterations=200, classAttr = "y")
dataTrain<-rbind(dataTrain,synth)

table(dataTrain$y) #comprobacion

## modelo
set.seed(46)
model<-randomForest::randomForest(as.factor(y)~., dataTrain, ntree=50, mtry=5, nodesize = 1)
predictedValues1 <- predict(model, newdata=dataTest, type="prob") # or "votes"
predictedValues <- predict(model, newdata=dataTest) # or "votes"

ma = table(dataTest$y, predictedValues)
ma
sum(diag(ma))/sum(ma) ##accuracy

model.roc <- roc(dataTest$y,predictedValues1[,2])
plot(model.roc, type="S", print.thres= 0.5,main=c("ROC Test",message),col="blue")


## predicciones validacion
predict_new <- predict(model, newdata = VALIDACION_X, type="prob")
predicciones_val <- cbind(predicciones_val, predict_new)
##

################################## predicciones final plataforma
predict_new <- predict(model, newdata = TEST, type="prob")
predicciones <- cbind(predicciones, predict_new)
##




prob_sum <- function(prediccionesOVO){
  clases <- unique(colnames(prediccionesOVO))
  sum_prob_clases <- sapply(clases, function(x){
    clase <- names(prediccionesOVO)== x
    sum_prob <- apply(prediccionesOVO[,clase], 1, sum)
    
  },USE.NAMES = TRUE)
  maximo <- apply(sum_prob_clases, 1, which.max)
  clases[maximo]
}

pred <- as.data.frame(predicciones)
clases_prediccion <- as.integer(prob_sum(pred))


id = TEST$id
prediccion <- as.data.frame(cbind(Id=id, Predicted=clases_prediccion))
write.csv(prediccion, file="OVO_v4.csv", sep=",", row.names = FALSE) ## quitar comillas en el excel

