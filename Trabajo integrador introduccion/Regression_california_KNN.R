#Data entry
setwd("C:/Users/Eilder Jorge/Documents/R Projects/Trabajo Integrador")
library(readr)
california <- read_csv("california/california.dat", col_names = FALSE, skip = 13)
datatypes<-scan("california/california.dat",what=character(),sep=" ",nlines=13)
names(california)<-datatypes[seq(4,30,by=3)]



#EDA
summary(california)
library(moments)
skewness(california[1:9])
kurtosis(california[1:9])


library(ggplot2)

library(tidyr)
california %>%
  gather(-MedianHouseValue, key = "var", value = "value") %>%
  ggplot(aes(x = MedianHouseValue, y = value)) +
  geom_bin2d() +
  stat_smooth() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

#Only MedianIncome has something of a linear relationship

require(kknn)
attach(california)
fitknn1 <- kknn(MedianHouseValue~.,california,california)
plot(MedianHouseValue,fitknn1$fitted.values)
yprime<-fitknn1$fitted.values
sqrt(sum((california$MedianHouseValue-yprime)^2)/length(yprime))

fitknn2 <- kknn(MedianHouseValue~MedianIncome+I(MedianIncome^2), california, california)
yprime=  fitknn2$fitted.values;  sqrt (sum(( MedianHouseValue-yprime )^2)/length(yprime ))
plot(MedianHouseValue,fitknn2$fitted.values)

setwd("C:/Users/Eilder Jorge/Documents/R Projects/Trabajo Integrador/california-5-fold")
nombre <- "california"

run_lm_fold <- function( i , x,  tt = "test") {
  file<- paste(x,"-5-",i,"tra.dat",sep ="")
  x_tra<- read.csv(file,  comment.char="@")
  file<- paste(x,"-5-",i,"tst.dat",sep ="")
  x_tst<- read.csv(file,  comment.char="@")
  In<- length(names( x_tra))- 1
  names(x_tra)[1:In] <- paste("X",1:In,sep ="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste("X", 1:In, sep ="")
  names(x_tst)[In+1] <- "Y"
  if(tt == "train") {
    test<-x_tra
  }
  else{
    test<-x_tst
  }
  fitMulti =lm(Y ~.,x_tra)
  yprime=predict( fitMulti,test )
  sum(abs(test$Y -yprime)^2)/length(yprime) ##MSE
}
lmMSEtrain<-mean(sapply (1:5,run_lm_fold,nombre ,"train"))
lmMSEtest <-mean(sapply (1:5,run_lm_fold,nombre,"test"))
lmMSEtrain
lmMSEtest

n <-length(names(california))-1
names(california)[1:n] <- paste ("X", 1:n,  sep ="")
names(california)[n+1] <- "Y"

fit1=lm(Y~X8+I(X8^2)+I(X8^3)+I(log(X3))+I(log(X4/X6))+I(log(X5/X6))+I(log(X6/X7))+I(log(X7)),california)
fit2=lm(Y~., california)
fit3=lm(Y~.+X4*X7*X8,  california)
fit4=lm(Y~.+I(X1^2)+I(X6^2)+I(X8^2)+I(X8^3)+I(X8^4)+X7*X8*X4*X5*X6, california)
summary(fit1)$ adj.r.squared
summary(fit2)$ adj.r.squared
summary(fit3)$ adj.r.squared
summary(fit4)$ adj.r.squared

run_lm_fold <- function( i , x,  tt = "test") {
  file<- paste(x,"-5-",i,"tra.dat",sep ="")
  x_tra<- read.csv(file,  comment.char="@")
  file<- paste(x,"-5-",i,"tst.dat",sep ="")
  x_tst<- read.csv(file,  comment.char="@")
  In<- length(names( x_tra))- 1
  names(x_tra)[1:In] <- paste("X",1:In,sep ="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste("X", 1:In, sep ="")
  names(x_tst)[In+1] <- "Y"
  if(tt == "train") {
    test<-x_tra
  }
  else{
    test<-x_tst
  }
  fitMulti=lm(Y~.+I(X1^2)+I(X6^2)+I(X8^2)+I(X8^3)+I(X8^4)+X7*X8*X4*X5*X6,x_tra)
  yprime=predict( fitMulti,test )
  sum(abs(test$Y -yprime)^2)/length(yprime) ##MSE
}
lmMSEtrain<-mean(sapply (1:5,run_lm_fold,nombre ,"train"))
lmMSEtest <-mean(sapply (1:5,run_lm_fold,nombre,"test"))
lmMSEtrain
lmMSEtest

run_knn_fold <- function( i , x,  tt = "test") {
  file<- paste(x,"-5-",i,"tra.dat",sep ="")
  x_tra<- read.csv(file,  comment.char="@")
  file<- paste(x,"-5-",i,"tst.dat",sep ="")
  x_tst<- read.csv(file,  comment.char="@")
  In<- length(names( x_tra))- 1
  names(x_tra)[1:In] <- paste("X",1:In,sep ="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste("X", 1:In, sep ="")
  names(x_tst)[In+1] <- "Y"
  if(tt == "train") {
    test<-x_tra
  }
  else{
    test<-x_tst
  }
  fitMulti=kknn(Y~.,x_tra,test)
  yprime=fitMulti$fitted.values
  sum(abs(test$Y -yprime)^2)/length(yprime) ##MSE
}
knnMSEtrain<-mean(sapply (1:5,run_knn_fold,nombre ,"train"))
knnMSEtest<-mean( sapply (1:5,run_knn_fold,nombre ," test"))
knnMSEtrain
knnMSEtest

setwd("C:/Users/Eilder Jorge/Documents/R Projects/Trabajo Integrador/")
resultados <- read.csv ("regr_test_alumnos.csv")
tablatst <- cbind(resultados [,2:dim( resultados )[ 2]])
colnames(tablatst)  <- names(resultados )[2:dim( resultados )[ 2]]
rownames (tablatst)  <- resultados [, 1]
#leemos la tabla con los erroresmediosde entrenamiento
resultados <- read.csv("regr_train_alumnos.csv ")
tablatra <- cbind(resultados [,2:dim( resultados )[2]])
colnames(tablatra )  <- names(resultados )[ 2:dim( resultados )[2]]
rownames (tablatra )  <- resultados [,1]

tablatra["california",]
tablatst["california",]

difs <- (tablatst[,1] - tablatst[,2]) / tablatst[,1]
wilc_1_2 <- cbind(ifelse (difs <0, abs( difs)+0.1, 0+0.1), ifelse (difs >0,  abs( difs)+0.1, 0+0.1))
colnames(wilc_1_2) <- c( colnames(tablatst)[ 1], colnames(tablatst)[ 2])
head(wilc_1_2)

LMvsKNNtst <-wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic
Rmas
Rmenos
pvalue

test_friedman <- friedman.test (as.matrix(tablatst))
test_friedman

tam  <- dim( tablatst)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatst),  groups,  p.adjust = "holm", paired = TRUE)
