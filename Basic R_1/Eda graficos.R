library(ggplot2)
#Ejercicio 1
stretch <- c(46,54,48,50,44,42,52)
distance <- c(148,182,173,166,109,141,166)
df<-data.frame(stretch,distance)
ggplot(df, aes(x=df[,2],y=df[,1]))+ geom_line() + labs(x="Stretch",y="Distance")

#Ejercicio 2
year<- as.character(seq(1970,1979,1))
snow.cover<-c(6.5,12.0,14.9,10.0,10.7,7.9,21.9,12.5,14.5,9.2)
df<-data.frame(year,snow.cover)

#a)  
ggplot(df, aes(x=df[,1],y=df[,2])) + geom_col() +labs(x="Year",y="Snow Cover")
#b)
ggplot(df,aes(x=df[,2]))+geom_histogram(binwidth = 3) +labs(x="Snow Cover")