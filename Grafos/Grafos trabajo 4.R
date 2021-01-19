library(bnlearn)
library(readr)
library(caret)

led <- read.csv("C:/Users/Eilder Jorge/Downloads/data/data/ledLXMn10.txt", sep=",")
led<-as.data.frame(led)
led<-led[,c(1,2,3,4,5,6,7,25)]

for(x in 1:8){
  led[,x]<-as.factor(led[,x])
}

set.seed(1)
index<-createDataPartition(led$class,list=FALSE,p=0.9)

train<-led[index,]
test<-led[-index,]

ledGS <- gs(train)
graphviz.plot(ledGS)
modelstring(ledGS)<-"[att1][att2][att3][att4][att5][att6][att7][class|att1:att2:att3:att4:att5:att6:att7]"
fitted<-bn.fit(ledGS,data=train)
pred<-predict(fitted,node="class",test)
print(paste("Accuracy: ",mean(pred==test$class)))


ledHC <- hc(train,score="mbde",iss=5)
graphviz.plot(ledHC)
fittedHC<-bn.fit(ledHC,data=train)
pred<-predict(fittedHC,node="class",test)
print(paste("Accuracy: ",mean(pred==test$class)))


ledX2 <-pc.stable(train,test="x2")
graphviz.plot(ledX2)
fittedX2<-bn.fit(ledX2,data=train)
pred<-predict(fittedX2,node="class",test)
print(paste("Accuracy: ",mean(pred==test$class)))