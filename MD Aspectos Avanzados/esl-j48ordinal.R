library(RWeka)

set.seed(2)
train=sample(1:nrow(iris), 100)
iris.test=iris[-train,]

train

modelC4.5 = J48(Species~., iris[train,])

#library(partykit)
#plot(modelC4.5)
modelC4.5.pred = predict(modelC4.5, iris.test,"probability")

evaluate_Weka_classifier(modelC4.5)
modelC4.5.pred
