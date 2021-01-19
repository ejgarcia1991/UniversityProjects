#Data entry
library(readr)
vowel <- read_csv("vowel/vowel.dat", col_names = FALSE, col_types = cols(X1 = col_integer(), X14 = col_factor(levels = c("0",  "1", "2", "3", "4", "5", "6",  "7", "8", "9", "10")), X2 = col_integer(), X3 = col_integer()), skip = 18)
vowel <- as.data.frame(vowel)
names_vowel<-scan("vowel/vowel.dat",what=character(),sep=" ",nlines=15)
names(vowel)<-names_vowel[seq(4,69,by=5)]
attach(vowel)

#EDA
summary(vowel) #cuartile, min, max, mean, median
apply(vowel[1:13],2,sd) #Standard deviation of the regressors

library(moments) #library necessary for skewness and kurtosis
apply(vowel[1:13],2,skewness) #skewness of the data, basically how condensed and with peaks it is
apply(vowel[1:13],2,kurtosis) #how assymetrical the data is
apply(vowel[1:13],2,shapiro.test) #normalization test

#Visual analysis
hist(vowel[[1]],breaks=2,main="Histogram of TT",xlab="TT") #histogram to check distribution

#Function to draw multiple plots at the same time using the grid library
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#Multiple bar plots of the categorical variables
library(ggplot2)
p1 <- ggplot(vowel, aes(x=vowel[[1]])) +
  geom_bar() +
  labs(title="Barplot of TT",x="TT") +
  theme_bw()

p2 <- ggplot(vowel, aes(x=vowel[[2]])) +
  geom_bar() +
  labs(title="Barplot of Speaker Number",x="Speaker number")+
  theme_bw()

p3 <- ggplot(vowel, aes(x=vowel[[3]])) +
  geom_bar() +
  labs(title="Barplot of Sex",x="Sex") +
  theme_bw()

p4 <- ggplot(vowel, aes(x=vowel[[8]])) +
  geom_histogram() +
  labs(title="Histogram of F5",x="F5") +
  theme_bw()

multiplot(p1,p2,p3,p4, cols=2)

#Box plot of the numerical continuous variables
boxplot(vowel[4:13])

#Normal distribution plots of half of the continuous variables
qqnorm(vowel[["F0"]],main="Q-Q plot of F0")
qqline(vowel[["F0"]])
qqnorm(vowel[["F1"]],main="Q-Q plot of F1")
qqline(vowel[["F1"]])
qqnorm(vowel[["F2"]],main="Q-Q plot of F2")
qqline(vowel[["F2"]])
qqnorm(vowel[["F3"]],main="Q-Q plot of F3")
qqline(vowel[["F3"]])
qqnorm(vowel[["F4"]],main="Q-Q plot of F4")
qqline(vowel[["F4"]])

#Cross tabulation for contingency table for relation between categorical variables
library(gmodels)
CrossTable(vowel[["Class"]], vowel[["TT"]], prop.t=TRUE, prop.r=TRUE,prop.c=TRUE)
CrossTable(vowel[["Class"]], vowel[["SpeakerNumber"]], prop.t=TRUE, prop.r=TRUE,prop.c=TRUE)
CrossTable(vowel[["Class"]], vowel[["Sex"]], prop.t=TRUE, prop.r=TRUE,prop.c=TRUE)

#Bar plot of all numerical continuous variables against Class to determine distribution and correlation visually
library(tidyr)
vowel %>%
  gather(-Class, -Sex, -TT, -SpeakerNumber, key = "var", value = "value") %>%
  ggplot(aes(x = Class, y = value)) +
  geom_col() +
  stat_smooth() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

# C.1
# Utilizar el algoritmo k-NN probando con diferentes valores de k. 
# Elegir el que considere más adecuado para su conjunto de datos. 
# Analice que ocurre en los valores de precisión en training 
# y test con los diferentes valores de k.

#Preprocessing needed to scale the continuous variables
table(vowel$Class)
vowel_n <- as.data.frame(lapply(vowel[,4:13], scale, center = TRUE, scale = TRUE))
boxplot(vowel_n) #checking if it worked as intended
plot(vowel[[14]],vowel_n[[1]])

#Knn
#load libraries and prepare file location
library(class)
library(caret)
nombre<- "vowel/vowel"

#Do knn with only training data
run_knn_fold_train <- function(i,x,k) {
  file<- paste(x,"-10-",i,"tra.dat",sep ="")
  x_tra<-read_csv(file, col_names = FALSE, col_types = cols(X1 = col_integer(), X14 = col_factor(levels = c("0",  "1", "2", "3", "4", "5", "6",  "7", "8", "9", "10")), X2 = col_integer(), X3 = col_integer()), skip = 18)
  In<- length(names( x_tra))-1
  names(x_tra)[1:In] <- paste("X",1:In,sep ="")
  names(x_tra)[In+1] <- "Y"
  #Normalize data and drop the three categorical variables with no contribution for knn algorithm
  x_tra_n <- as.data.frame(lapply(x_tra[,4:13], scale, center = TRUE, scale = TRUE))

  #Do knn only on training data
  knn_test_pred <- knn(train = x_tra_n, test = x_tra_n, cl = x_tra$Y, k=k)
  postResample(pred = knn_test_pred, obs = x_tra$Y)[1] #Accuracy column
}
#helper function to simply doing 10 cv with 10 different k values for knn
run_knn_fold_train_different_k <- function(k){
  knnMSEtrain<-mean(sapply(1:10,run_knn_fold_train,nombre,k))
  knnMSEtrain
}

train_values<-sapply(1:10,run_knn_fold_train_different_k)
train_values #Knn values with different K using CV, most accurate is K=1, but it's likely to be overfitting
#[1] 1.0000000 0.9950617 0.9930415 0.9810325 0.9792368 0.9652076 0.9626263
#[8] 0.9424242 0.9380471 0.9139169

#Proper Knn with both train and test data
run_knn_fold_test <- function(i,x,k) {
  file<- paste(x,"-10-",i,"tra.dat",sep ="")
  x_tra<-read_csv(file, col_names = FALSE, col_types = cols(X1 = col_integer(), X14 = col_factor(levels = c("0",  "1", "2", "3", "4", "5", "6",  "7", "8", "9", "10")), X2 = col_integer(), X3 = col_integer()), skip = 18)
  file<- paste(x,"-10-",i,"tst.dat",sep ="")
  x_tst<-read_csv(file, col_names = FALSE, col_types = cols(X1 = col_integer(), X14 = col_factor(levels = c("0",  "1", "2", "3", "4", "5", "6",  "7", "8", "9", "10")), X2 = col_integer(), X3 = col_integer()), skip = 18)
  In<- length(names( x_tra))-1
  names(x_tra)[1:In] <- paste("X",1:In,sep ="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste("X",1:In, sep ="")
  names(x_tst)[In+1] <- "Y"
  
  #Normalize data and drop the three categorical variables with no contribution for knn algorithm
  x_tra_n <- as.data.frame(lapply(x_tra[,4:13], scale, center = TRUE, scale = TRUE))
  x_tst_n <- as.data.frame(lapply(x_tst[,4:13], scale, center = TRUE, scale = TRUE))

  #Do Knn and then compare with test data
  knn_test_pred <- knn(train = x_tra_n, test = x_tst_n, cl = x_tra$Y, k=k)
  postResample(pred = knn_test_pred, obs = x_tst$Y)[1] #Accuracy column
}

#Helper function
run_knn_fold_test_different_k <- function(k,type){
  knnMSEtest<-sapply(1:10,run_knn_fold_test,nombre,k)
  knnMSEtest
}
#Obtaining all KNN models accuracy for K between 1 and 10 for each partition
test_values<-sapply(1:10,run_knn_fold_test_different_k)
#Obtain the mean and standard deviation for each tested K
knn_means<-apply(test_values,2,mean)
knn_sd<-apply(test_values,2,sd)
knn_means
knn_sd
#Save 5 KNN models with K=1,3,5,7,10 for comparison with other models for each test
tests_knn<-test_values[,c(1,3,5,7,10)]
tests_knn

#For each K in knn from 1 to 10 we get the following accuracy
#0.9909091 0.9717172 0.9696970 0.9414141
#0.9373737 0.9272727 0.9131313 0.8888889
#0.8777778 0.8363636

#And the following standard deviation
#0.01004474 0.01831848 0.02233417
#0.02064607 0.03222488 0.02418624
#0.03305842 0.02735366 0.05225910
#0.05233497

plot(knn_means, type="l", xlab="K", ylab="accuracy")


#Knn using Caret for double confirmation
knnFit <- train(vowel[,4:13], vowel[,14],
                method = "knn",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv")
                )
knnFit
#K tested from 5 to 23, best model is K=5 with an approximate accuracy of 94.2%

# C.2
# Utilizar el algoritmo LDA para clasificar. No olvide comprobar las asunciones.
#LDA
library(MASS)
#LDA assumptions, normality, randomness, samples 5 to 10x more than predictors, common variance
# normality
apply(vowel[4:13],2,shapiro.test) #fail, need to normalize
# Assuming observations are random and uncorrelated...
# 5 to 10 more samples than predictors
dim(vowel_n) #99 times more, 10 times more after being separated into folds
# Predictors have a common variance
var(vowel_n)

# Linear Discriminant Analysis by hand using 10 fold cross validation
run_lda_fold <- function(i,x) {
  #Load train and test data of the current fold
  file<- paste(x,"-10-",i,"tra.dat",sep ="")
  x_tra<-read_csv(file, col_names = FALSE, col_types = cols(X1 = col_integer(), X14 = col_factor(levels = c("0",  "1", "2", "3", "4", "5", "6",  "7", "8", "9", "10")), X2 = col_integer(), X3 = col_integer()), skip = 18)
  file<- paste(x,"-10-",i,"tst.dat",sep ="")
  x_tst<-read_csv(file, col_names = FALSE, col_types = cols(X1 = col_integer(), X14 = col_factor(levels = c("0",  "1", "2", "3", "4", "5", "6",  "7", "8", "9", "10")), X2 = col_integer(), X3 = col_integer()), skip = 18)
  In<- length(names( x_tra))-1
  names(x_tra)[1:In] <- paste("X",1:In,sep ="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste("X",1:In, sep ="")
  names(x_tst)[In+1] <- "Y"
  #Normalize data
  x_tra[4:13] <- as.data.frame(lapply(x_tra[,4:13], scale, center = TRUE, scale = TRUE))
  x_tst[4:13] <- as.data.frame(lapply(x_tst[,4:13], scale, center = TRUE, scale = TRUE))
  #drop predictors with 0 variance and near 0 correlation
  x_tra<-x_tra[-c(1,2,3)]
  #Do the algorithm calculations
  lda.fit <- lda(Y~.-Y,data=x_tra)
  #Predicted values against test data
  ld.pred<-predict(lda.fit,x_tst)
  #Return the mean of how many predicted values are correct as an accuracy metric
  mean(ld.pred$class==x_tst$Y)
}
lda_cv<-sapply(1:10,run_lda_fold,nombre) #Obtain the mean accuracy on each fold
mean(lda_cv) #average them to obtain the final accuracy of the algorithm
#0.6171717
sd(lda_cv) #standard deviaton to determine error
#0.04108572

# Linear Discriminant Analysis with caret for double confirmation
ldaFit <- train(vowel[,4:13], vowel[,14],
                method = "lda",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"))
ldaFit
#0.5959596

# C.3 
# Utilizar el algoritmo QDA para clasificar. No olvide comprobar las asunciones.

#QDA
# Check same variance but this time for each class
var_l<-vector("numeric",11)
for(x in 1:11){
  v_0<-sapply(vowel[vowel$Class == as.character(x-1),][4:13], var)
  var_l[x]<-mean(v_0)
}
var_l

# Quadratic Discriminant Analysis by hand using 10 fold cross validation
run_qda_fold <- function(i,x) {
  #Load train and test data of the current fold
  file<- paste(x,"-10-",i,"tra.dat",sep ="")
  x_tra<-read_csv(file, col_names = FALSE, col_types = cols(X1 = col_integer(), X14 = col_factor(levels = c("0",  "1", "2", "3", "4", "5", "6",  "7", "8", "9", "10")), X2 = col_integer(), X3 = col_integer()), skip = 18)
  file<- paste(x,"-10-",i,"tst.dat",sep ="")
  x_tst<-read_csv(file, col_names = FALSE, col_types = cols(X1 = col_integer(), X14 = col_factor(levels = c("0",  "1", "2", "3", "4", "5", "6",  "7", "8", "9", "10")), X2 = col_integer(), X3 = col_integer()), skip = 18)
  In<- length(names( x_tra))-1
  names(x_tra)[1:In] <- paste("X",1:In,sep ="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste("X",1:In, sep ="")
  names(x_tst)[In+1] <- "Y"
  #normalize them
  x_tra[4:13] <- as.data.frame(lapply(x_tra[,4:13], scale, center = TRUE, scale = TRUE))
  x_tst[4:13] <- as.data.frame(lapply(x_tst[,4:13], scale, center = TRUE, scale = TRUE))
  #Do the algorithm calculations
  qda.fit <-qda(Y~.-Y, data=x_tra)
  #Test
  qda.pred <- predict(qda.fit,x_tst)
  #Return the mean of the test as an accuracy metric
  mean(qda.pred$class==x_tst$Y)
}
qda_cv<-sapply(1:10,run_qda_fold,nombre) #Obtain the mean accuracy on each fold
mean(qda_cv) #average them to obtain the final accuracy of the algorithm
#0.9111111
sd(qda_cv) #standard deviaton to determine error
#0.03393833 

#Quadratic Discriminant Analysis with caret for double confirmation
qdaFit <- train(vowel[,1:13], vowel$Class,
                method = "qda",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"))
qdaFit
#0.9171717

# C.4
# Comparar los resultados de los tres algoritmos.

tests_knn
lda_cv
qda_cv
comparison_table<-cbind(tests_knn,lda_cv,qda_cv)
colnames(comparison_table)<-c("KNN 1", "KNN 3", "KNN 5", "KNN 7", "KNN 10","LDA","QDA")
rownames(comparison_table)<-paste("Test ",1:10)
comparison_table

#Wilcoxon test
#No need to scale the error in classification as all belong in the range 0~1.
#Best KNN with K=1 against LDA
LMvsKNNtst <-wilcox.test(comparison_table[,1], comparison_table[,6], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic #This is the value for Linear regression model
pvalue <- LMvsKNNtst$p.value #This is the p-value of the comparison
LMvsKNNtst <- wilcox.test(comparison_table[,6], comparison_table[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic #This is the value for K-nn model
Rmas #55
Rmenos #0
pvalue #0.005857099
#We can assess with total confidence that they are indeed different

#Best KNN with K=1 against QDA
LMvsKNNtst <-wilcox.test(comparison_table[,1], comparison_table[,7], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic #This is the value for Linear regression model
pvalue <- LMvsKNNtst$p.value #This is the p-value of the comparison
LMvsKNNtst <- wilcox.test(comparison_table[,7], comparison_table[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic #This is the value for K-nn model
Rmas #55
Rmenos #0
pvalue #0.005857099
#We can assess with total confidence that they are indeed different, it's surprising that the numbers are identical,
#but just goes to show how much Knn with K=1 dominates the comparison

#Best KNN with LDA against QDA
LMvsKNNtst <-wilcox.test(comparison_table[,6], comparison_table[,7], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic #This is the value for Linear regression model
pvalue <- LMvsKNNtst$p.value #This is the p-value of the comparison
LMvsKNNtst <- wilcox.test(comparison_table[,7], comparison_table[,6], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic #This is the value for K-nn model
Rmas #0
Rmenos #55
pvalue #0.005825024
#Similar to the cases above, we are very confident that they perform differently.

#Friedman training data algorithm comparison
test_friedman <- friedman.test(as.matrix(comparison_table))
test_friedman #chi-squared = 55.211, p-value=4.203e-10, significantly inferior to 0.05 which shows at least one algorithm 
#is very different from another

#Holm algorithm comparison
#Prepare the groups
tam  <- dim(comparison_table)
groups <- rep(1:tam[2], each=tam[1])
#Do the test
pairwise.wilcox.test(as.matrix(comparison_table),  groups,  p.adjust = "holm", paired = TRUE)
#     1     2     3     4     5     6    
#  2 0.104 -     -     -     -     -    
#  3 0.104 0.104 -     -     -     -    
#  4 0.104 0.104 0.104 -     -     -    
#  5 0.041 0.104 0.104 0.104 -     -    
#  6 0.104 0.104 0.041 0.041 0.104 -    
#  7 0.104 0.104 0.104 0.905 0.104 0.104
#  1-5 3-6 4-6, so K-nn with K=1 and K=10, and LDA with Knn k=5 and K = 7 are different.
apply(comparison_table,2,mean)
