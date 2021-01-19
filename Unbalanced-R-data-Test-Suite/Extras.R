set.seed(324)

CVIndices <- createFolds(dataset$Class, k=5)
#First partition
dataTrain <- dataset[-testIndices[[1]],]
dataTest  <- dataset[testIndices[[1]],]



#SmoteFamily::ADASYN(train,train$y,k=5)
#SmoteFamily::ANS(train,train$y,dupsize=0)
#SmoteFamily::BLSMOTE(train,train$y,K=5,C=5,dupsize=0, method = "type2")
#SmoteFamily::DBSMOTE(train,train$y,dupsize=0, eps=NULL)
#SmoteFamily::RSLS(train,train$y,K=5,C=5,dupsize=0)
#SmoteFamily::SLS(train,train$y,K=5,C=5,dupsize=0)
#SmoteFamily::SMOTE(train,train$y,K=5,dupsize=0)

#imbalance::mwmote(train,numInstances=1000,kNoisy = 5,kMajority = 3,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
#imbalance::neater(train,extendedtrain,k=3,iterations=100,classAttr = "y")
#imbalance::oversample(train,ratio=0.2,method="method",filtering=TRUE,classAttr = "y")

#ROSE::ovun.sample(formula, train, method="both", N, p=0.5, seed=1)
#ROSE::ROSE(formula, data, N, p=0.5, hmult.majo=1, hmult.mino=1, seed=1)

#data<-ubBalance(X= input, Y=output, ubConf data)
#balancedData<-cbind(data$X,data$Y)
#data<-ubNCL(X=input, Y= output)
#newData<-cbind(data$X, data$Y)
#data<-ubOSS(X=input, Y= output)
#newData<-cbind(data$X, data$Y)
#data<-ubTomek(X=input, Y= output)
#newData<-cbind(data$X, data$Y)


#noiseFiltersR::AENN(train, k = 5)
#noiseFiltersR::BBNR(train, k = 3)
#noiseFiltersR::C45iteratedVotingFilter(train, nfolds = 10)
#noiseFiltersR::CNN(train)
#noiseFiltersR::CVCF(train, nfolds = 10)
#noiseFiltersR::dynamicCF(train, nfolds = 10,m=3)
#noiseFiltersR::edgeBoostFilter(train, m = 15, percent = 0.05, threshold = 0)
#noiseFiltersR::ENN(train, k = 3)
#noiseFiltersR::GE(train, k = 5, kk = ceiling(k/2))
#noiseFiltersR::HARF(train, nfolds = 10, agreementLevel = 0.7, ntrees = 500)
#noiseFiltersR::hybridRepairFilter(train, noiseAction = "repair")
#noiseFiltersR::IPF(train, nfolds = 5, p = 0.01, s = 3,y = 0.5)
#noiseFiltersR::ORBoostFilter(train, N = 20, d = 11, Naux = max(20, N))
#noiseFiltersR::PF(x, nfolds = 5, consensus = FALSE, p = 0.01, s = 3, y = 0.5, theta = 0.7)
#noiseFiltersR::PRISM(train)

#noiseFiltersR::ModeFilter(train, type = "classical", noiseAction = "repair", epsilon = 0.05, maxIter = 100, alpha = 1, beta = 1)


#RandomForest::combine (combinar random forests)
#RandomForest::outlier (deteccion de outliers)
#RandomForest::predict(object, newdata, type="prob", norm.votes=TRUE, cutoff, ...)
#RandomForest::randomForest(x=train, y=class, ntree=500, mtry=6, nodesize = 1)
#rf<-randomForest(x=train[,1:29], y=train$y, ntree=500, mtry=6, nodesize = 1,importance=TRUE)
#tuneRF(x=train[,1:29], y=train$y,stepFactor=2,improve=0.01)
#varUsed(rf)

set.seed(seed)
train<-smotefamily::ADAS(train[,1:29],train$y,K=2)
train<-train$data
colnames(train)[30]<-"y"
train$y<-as.factor(train$y)

set.seed(seed)
train<-smotefamily::BLSMOTE(train[,1:29],train$y,K=2,C=2, method = "type1")
train<-train$data
colnames(train)[30]<-"y"
train$y<-as.factor(train$y)

set.seed(seed)
train<-smotefamily::RSLS(train[,1:29],train$y,K=5,C=5)
train<-train$data
colnames(train)[30]<-"y"
train$y<-as.factor(train$y)

set.seed(seed)
train<-smotefamily::SLS(train[,1:29],train$y,K=5,C=5)
train<-train$data
colnames(train)[30]<-"y"
train$y<-as.factor(train$y)

set.seed(seed)
synth<-imbalance::mwmote(train,numInstances=nrow(train)/proportion,kNoisy = 5,kMajority =3 ,threshold = 5,cmax = 2,cclustering = 3,classAttr = "y")
synth<-imbalance::neater(train,synth,k=3,iterations=200,classAttr = "y")
train<-rbind(train,synth)

set.seed(seed)
synth<-ROSE::ovun.sample(y~., train, method="both", p=0.2, seed=seed)
train<-synth$data

set.seed(seed)
synth<-ROSE::ovun.sample(y~., train, method="under", p=0.2, seed=seed)
train<-synth$data

set.seed(seed)
synth<-ubNCL(X=train[,1:29], Y=train$y)
train<-cbind(synth$X, synth$Y)
colnames(train)[30]<-"y"

set.seed(seed)
synth<-ubTomek(X=train[,1:29], Y=train$y)
train<-cbind(synth$X, synth$Y)
colnames(train)[30]<-"y"

set.seed(seed)
synth<-C45iteratedVotingFilter(train, nfolds = 10)
train<-synth$cleanData

set.seed(seed)
synth<-AENN(train, k = 2)
train<-synth$cleanData

set.seed(seed)
synth<-edgeBoostFilter(train, m = 15, percent = 0.05, threshold = 0)
train<-synth$cleanData

set.seed(seed)
synth<-IPF(train, nfolds = 5, p = 0.01, s = 3,y = 0.5)
train<-synth$cleanData

set.seed(seed)
synth<-PRISM(train)
train<-synth$cleanData