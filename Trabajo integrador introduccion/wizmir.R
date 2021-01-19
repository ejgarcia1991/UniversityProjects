#Data entry
library(readr)
wizmir <- read_csv("wizmir/wizmir.dat", col_names = FALSE, comment = "@")
names_wizmir<-scan("wizmir/wizmir.dat",what=character(),sep=" ",nlines=11)
names(wizmir)<-names_wizmir[seq(4,32,by=3)]
attach(wizmir)

#EDA
summary(wizmir) #cuartile, min, max, mean, median
apply(wizmir,2,sd) #Standard deviation of the regressors

library(moments) #library necessary for skewness and kurtosis
apply(wizmir,2,skewness) #skewness of the data, basically how condensed and with peaks it is
apply(wizmir,2,kurtosis) #how assymetrical the data is
apply(wizmir,2,shapiro.test) #normalization test
apply(wizmir[1:9],2,cor,wizmir[[10]]) #correlation between regressors and the target variable

#Visual analysis
hist(wizmir[["Precipitation"]],main = "Histogram of Precipitation",xlab="Precipitation",breaks=100) #histogram to check distribution

#Multipe Q-Q plots to visualize the normalization of the data
qqnorm(as.matrix(wizmir["Standard_pressure"]),main="Q-Q Plot of Standard pressure")
qqline(as.matrix(wizmir["Standard_pressure"]))
qqnorm(as.matrix(wizmir["Wind_speed"]),main="Q-Q Plot of Wind Speed")
qqline(as.matrix(wizmir["Wind_speed"]))
qqnorm(as.matrix(wizmir["Max_temperature"]),main="Q-Q Plot of Max temperature")
qqline(as.matrix(wizmir["Max_temperature"]))
qqnorm(as.matrix(wizmir["Min_temperature"]),main="Q-Q Plot of Min temperature")
qqline(as.matrix(wizmir["Min_temperature"]))

#Visual correlation of the regressors with the target variable.
library(ggplot2)
library(tidyr)
wizmir %>%
  gather(-Mean_temperature, key = "var", value = "value") %>%
  ggplot(aes(x = Mean_temperature, y = value)) +
  geom_bin2d() +
  stat_smooth() +
  facet_wrap(~ var, scales = "free") +
  labs(title="Correlation of Mean_temperature with other variables") +
  theme_bw()

#R.1
#Utilizar el algoritmo de regresión lineal simple sobre cada regresor para obtener los modelos.
#Si el dataset contiene más de 5, usar las 5 más relevantes. 
#Después escoger el más adecuado según las medidas de calidad.
#Max_temperature has the best correlation and is a pretty normalized regressor
fit1=lm(Mean_temperature~Max_temperature) 
summary(fit1) 
plot(Mean_temperature~Max_temperature)
abline(fit1,col="red")
sqrt(sum(abs(Mean_temperature -fit1$fitted.values)^2)/length(fit1$fitted.values))
#0.9576 Adjusted R squared
#2.957531 RMSE
#P-value below 0.05 and a small adjusted R squared of 0.9576 alongside 2.957531 RMSE shows this is a good model using only one regressor

fit2=lm(Mean_temperature~Min_temperature)
summary(fit2)
plot(Mean_temperature~Min_temperature)
abline(fit2,col="red")
sqrt(sum(abs(Mean_temperature -fit2$fitted.values)^2)/length(fit2$fitted.values))
#0.919 Adjusted R squared
#4.08958 RMSE

fit3=lm(Mean_temperature~Dewpoint)
summary(fit3)
plot(Mean_temperature~Dewpoint)
abline(fit3,col="red")
sqrt(sum(abs(Mean_temperature -fit3$fitted.values)^2)/length(fit3$fitted.values))
#0.6152 Adjusted R squared
#8.911483 RMSE

fit4=lm(Mean_temperature~Sea_level_pressure)
summary(fit4)
plot(Mean_temperature~Sea_level_pressure)
abline(fit4,col="red")
#0.3394 Adjusted R squared
#No need to further calculate when previous models are superior

fit5=lm(Mean_temperature~Visibility)
summary(fit5)
plot(Mean_temperature~Visibility)
abline(fit5,col="red")
#0.05042 Adjusted R squared

#R.2
#Utilizar el algoritmo de regresión lineal múltiple. Justificar si el modelo es mejor que el anterior.
#Tener en cuenta posibles interacciones y no linealidad.
#Classic combination of two linear regressors with high correlation to the target variable
fit6=lm(Mean_temperature~Min_temperature+Max_temperature) 
summary(fit6)
sqrt(sum(abs(Mean_temperature -fit6$fitted.values)^2)/length(fit6$fitted.values))
#0.9915
#1.320711 RMSE
#P-value below 0.05 and a small adjusted R squared of 0.9915 alongside 1.320711 RMSE shows this is a good model using only two regressors

#All inclusive model
fit7=lm(Mean_temperature~.,data=wizmir)
summary(fit7)
sqrt(sum(abs(Mean_temperature -fit7$fitted.values)^2)/length(fit7$fitted.values))
#0.9924
#1.251833 RMSE
#Lowest error but uses all variables, could lead to overfitting or an hard to understand model

#Removing variables with low contribution
fit8=lm(Mean_temperature~.-Precipitation -Wind_speed,data=wizmir)
summary(fit8)
#0.9924

#Further removing
fit9=lm(Mean_temperature~.-Precipitation -Wind_speed -Standard_pressure - Max_wind_speed,data=wizmir)
summary(fit9)
sqrt(sum(abs(Mean_temperature -fit9$fitted.values)^2)/length(fit9$fitted.values))
#0.9923
#1.257494 RMSE
#Best model found in this search, very low error and includes 4 less regressors than the multiple model. 

#Further removing, increase in error
fit10=lm(Mean_temperature~.-Dewpoint - Sea_level_pressure -Precipitation -Wind_speed -Standard_pressure - Max_wind_speed,data=wizmir)
summary(fit10)
#0.9917

#Testing with non lineal interactions
fit11=lm(Mean_temperature~Min_temperature+Max_temperature+Dewpoint+I(Dewpoint^2)+I(log(Sea_level_pressure+Visibility)),data=wizmir)
summary(fit11)
sqrt(sum(abs(Mean_temperature -fit11$fitted.values)^2)/length(fit11$fitted.values))
#0.9923
#1.271545 RMSE

#Further testing
fit12=lm(Mean_temperature~Min_temperature+Max_temperature+log(Dewpoint)+Sea_level_pressure+Visibility,data=wizmir)
summary(fit12)
sqrt(sum(abs(Mean_temperature -fit11$fitted.values)^2)/length(fit11$fitted.values))
#0.9923
#1.271545 RMSE

#Other tests deleted in order to avoid bloating code

#R.3
#Aplicar el algoritmo k-NN para regresión
#Load k-nn library and setup location of the folder containing the folds for cross validation
library(kknn)
nombre<-"wizmir/wizmir"
#Function to do K-nn manually over the folds
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
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
knnMSEtrain<-mean(sapply (1:5,run_knn_fold,nombre ,"train")) #train accuracy average over the 5 folds
knnMSEtest<-mean( sapply (1:5,run_knn_fold,nombre ,"test")) #test accuracy average over the 5 folds
knnMSEtrain #2.5383
knnMSEtest #6.060107

#Function to do linear regression manually over the folds
run_lm_fold <- function( i , x,  tt = "test", formula) {
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
  fitMulti =lm(formula,x_tra)
  yprime=predict( fitMulti,test )
  sum(abs(test$Y -yprime)^2)/length(yprime) ##MSE
}
lmMSEtrain<-mean(sapply (1:5,run_lm_fold,nombre ,"train",Y~.)) #train accuracy average over the 5 folds for best model
lmMSEtest <-mean(sapply (1:5,run_lm_fold,nombre,"test",Y~.)) #test accuracy average over the 5 folds for best model
lmMSEtrain #1.564527
lmMSEtest #1.604908

lmMSEtrain<-mean(sapply (1:5,run_lm_fold,nombre ,"train",Y~X1+X2)) #train accuracy average over the 5 folds for simple lineal model
lmMSEtest <-mean(sapply (1:5,run_lm_fold,nombre,"test",Y~X1+X2)) #test accuracy average over the 5 folds for simple lineal
lmMSEtrain #1.744869
lmMSEtest #1.751218


lmMSEtrain<-mean(sapply (1:5,run_lm_fold,nombre ,"train",Y~. -X4 -X6 -X8 -X9)) #train accuracy average over the 5 folds for 5 regressors model
lmMSEtest <-mean(sapply (1:5,run_lm_fold,nombre,"test",Y~.-X4 -X6 -X8 -X9)) #test accuracy average over the 5 folds for 5 regressors model
lmMSEtrain #1.579781
lmMSEtest #1.609519




#R.4
#Comparar los resultados de los dos algoritmos de regresión múltiple entre sí.
#Comparar con los resultados del M5 que están en la tabla de resultados disponibles.
#Remplazar en el .csv con los valores propios y usar Wilcoxon, Friedman y Holms.

#load data from the .csv with the different accuracy results from the algorithms applied over different datasets
#Training data accuracy
resultados <- read.csv("regr_train_alumnos.csv")
tablatra <- cbind(resultados[,2:dim( resultados)[2]])
colnames(tablatra )  <- names(resultados)[2:dim(resultados)[2]]
rownames (tablatra )  <- resultados [,1]

#Test data accuracy
resultados <- read.csv ("regr_test_alumnos.csv")
tablatst <- cbind(resultados [,2:dim(resultados)[2]])
colnames(tablatst)  <- names(resultados)[2:dim(resultados)[2]]
rownames (tablatst)  <- resultados [,1]

#Wilcoxon test
#Need to normalize the error in regression, here we normalize for comparison between Linear regression and K-NN
difs <- (tablatra[,2] - tablatra[,1]) / tablatra[,2]
#We make a table containing two columns depending on the normalized difference above. If the difference is negative, we assign the absolute value
#of the difference to the comparison algorithm +0.1, in this case K-NN, or 0+0.1 if it's positive. Then we do the reverse for the algorithm we took as
#base for the comparison, if it's positive we take the difference+0.1 and 0+0.1 otherwise. The 0.1 is due to wilcoxon calculations below
wilc_1_2 <- cbind(ifelse(difs <0, abs( difs)+0.1, 0+0.1), ifelse(difs>0,  abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2) <- c( colnames(tablatra)[2], colnames(tablatra)[1])
wilc_1_2

#The actual wilcoxon test, where we define what algorithms we want to compare
LMvsKNNtst <-wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic #This is the value for Linear regression model
pvalue <- LMvsKNNtst$p.value #This is the p-value of the comparison
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic #This is the value for K-nn model
Rmas #168
Rmenos #3
pvalue #3.814697e-05
#1-pvalue gives us an extremely high confidence value that they perform differently, not to mention the distance between R+ and R-

#Same Wilcoxon test but applied over test data instead of train data and this time with K-nn as main comparison algorithm
difs <- (tablatst[,1] - tablatst[,2]) / tablatst[,1]
wilc_1_2 <- cbind(ifelse (difs <0, abs( difs)+0.1, 0+0.1), ifelse (difs >0,  abs( difs)+0.1, 0+0.1))
colnames(wilc_1_2) <- c( colnames(tablatst)[1], colnames(tablatst)[2])
wilc_1_2

LMvsKNNtst <-wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic
Rmas #78
Rmenos #93
pvalue #0.7660294
#This time we get a 0.234*100%= 23.4% confidence that they are different, so we can conclude there isn't any major difference

#Friedman training data algorithm comparison
test_friedman <- friedman.test(as.matrix(tablatra))
test_friedman #chi-squared = 20.333, p-value=3.843e-05, significantly inferior to 0.05 which shows at least one algorithm 
              #is very different from another

#Friedman test data algorithm comparison
test_friedman <- friedman.test(as.matrix(tablatst))
test_friedman #chi-squared = 8.4444, p-value=0.01467, inferior to 0.05 which shows at least one algorithm is very different from another

#Holm training data algorithm comparison
#Prepare the groups
tam  <- dim(tablatra)
groups <- rep(1:tam[2], each=tam[1])
#Do the test
pairwise.wilcox.test(as.matrix(tablatra),  groups,  p.adjust = "holm", paired = TRUE)
#       1      2     
#  2 0.0031 -     
#  3 0.0032 0.0032
# All are very similarly different, no noticeable advantages


#Holm test data algorithm comparison
#Prepare the groups
tam  <- dim( tablatst)
groups <- rep(1:tam[2], each=tam[1])
#Do the test
pairwise.wilcox.test(as.matrix(tablatst),  groups,  p.adjust = "holm", paired = TRUE)
#       1     2    
#  2 0.580 -    
#  3 0.081 0.108
#  M5 has a distinct advantage over both K-nn and Lm with ~90% confidence, meanwhile K-nn and Lm are very similar