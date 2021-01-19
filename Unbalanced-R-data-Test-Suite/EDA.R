#EDA a pie chart is best
n_classes <- c(table(dataset$y)[[1]],table(dataset$y)[[2]],table(dataset$y)[[3]],table(dataset$y)[[4]],table(dataset$y)[[5]],table(dataset$y)[[6]],table(dataset$y)[[7]],table(dataset$y)[[8]],table(dataset$y)[[9]])
pct <- round(n_classes/sum(n_classes)*100,digits=2)

lbls <- levels(dataset$y)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels

pie(n_classes,labels = lbls, main="Class distribution")
pie


x <- dataset[,1:29]
y <- dataset[,30]
featurePlot(x=x, y=y, plot="box")

library(ggvis)
dataset %>% ggvis(~X_1, ~X_2, fill = ~y) %>% layer_points()
