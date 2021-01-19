library(ggplot2)
library(readr)
AUDCAD<- read_csv("FOREX DATA/3-PROCESSED/HOURLY/AUDCAD.csv")
AUDCAD<-as.data.frame(AUDCAD)
AUDUSD<- read_csv("FOREX DATA/3-PROCESSED/DAILY/AUDUSD.csv")
AUDUSD<-as.data.frame(AUDUSD)
AUDJPY<- read_csv("FOREX DATA/2-CLEANED/HOURLY/AUDJPY_2.csv")
AUDJPY<-as.data.frame(AUDJPY)
EURUSD<- read_csv("FOREX DATA/2-CLEANED/HOURLY/EURUSD_2.csv")
EURUSD<-as.data.frame(EURUSD)
GBPJPY<- read_csv("FOREX DATA/2-CLEANED/HOURLY/GBPJPY_2.csv")
GBPJPY<-as.data.frame(GBPJPY)


t<-head(AUDCAD$Open,200)

plot(t,type="l",col="green")
lines(calculateMeanOfTemporalSeries(t,15))
lines(calculateMeanOfTemporalSeries(t,100))

lines(calculateExponentialMeanOfTemporalSeries(t,20))
lines(calculateWMAOfTemporalSeries(t,20))

ZScore<-calculateZScoreOfTemporalSeries(t,50)
plot(ZScore,type="l")
lines(calculateExponentialMeanOfTemporalSeries(ZScore,20))


ZScore
calculateExponentialMeanOfTemporalSeries(ZScore,10)
plot(calculateExponentialMeanOfTemporalSeries(ZScore,10),type="l")
lines(rep(0,200))
lines(calculateExponentialMeanOfTemporalSeries(ZScore,10))

skip<-which(AUDCAD[,1]==head(AUDUSD[,1],75407))
AUDCAD[-skip,]

datas<-as.data.frame(cbind(head(AUDCAD$Close,50000),head(AUDJPY$Close,50000),head(EURUSD$Close,50000),head(GBPJPY$Close,50000)))

datas<-as.data.frame(cbind(head(AUDCAD$Close,3000),head(AUDJPY$Close,3000),head(EURUSD$Close,3000),head(GBPJPY$Close,3000)))

datas<-as.data.frame(cbind(head(AUDCAD$Close,630),head(AUDJPY$Close,630),head(EURUSD$Close,630),head(GBPJPY$Close,630)))


cor(AUDJPY$Close,AUDJPY$Open)

par(mfrow=c(2,2))
boxplot(datas$V1,main="AUDCAD boxplot")
boxplot(datas$V2,main="AUDJPY boxplot")
boxplot(datas$V3,main="EURUSD boxplot")
boxplot(datas$V4,main="GBPJPY boxplot")

hAC<-head(AUDCAD$Close,630)
hAJ<-head(AUDJPY$Close,630)
hEU<-head(EURUSD$Close,630)
hGJ<-head(GBPJPY$Close,630)


zAC<-calculateZScoreOfTemporalSeries(hAC,8)
zAJ<-calculateZScoreOfTemporalSeries(hAJ,8)
zEU<-calculateZScoreOfTemporalSeries(hEU,8)
zGJ<-calculateZScoreOfTemporalSeries(hGJ,8)



boxplot(zAC,ylim = c(-5, 5),main="AUDCAD Z-Score boxplot")
boxplot(zAJ,ylim = c(-5, 5),main="AUDJPY Z-Score boxplot")
boxplot(zEU,ylim = c(-5, 5),main="EURUSD Z-Score boxplot")
boxplot(zGJ,ylim = c(-5, 5),main="GBPJPY Z-Score boxplot")

zGJ[zGJ>10]<-3
zAJ[zAJ>10]


?which

hist(zAC,main="AUDCAD Z-Score histogram")
hist(zAJ,main="AUDJPY Z-Score histogram")
hist(zEU,main="EURUSD Z-Score histogram")
hist(zGJ,main="GBPJPY Z-Score histogram")

cor(zAC,zGJ)



summary(AUDCAD$Close)
cor(AUDCAD$Volume,AUDCAD$Close, method="pearson")

cor(zScore,zScoreOpen, method="pearson")



plot(AUDCAD$Close,type="line")

plot(head(AUDCAD$Volume,2000),head(AUDCAD$Close,2000),xlab="Open",ylab="Close")

ggplot(start, aes(x=Low, y=Close)) + 
  geom_point()+
  geom_smooth(method=lm)

start$`Local time`

p=ggplot() +
  geom_line(data = temp, aes(x=`Local time`,y=zScore) , color = "blue") +
  geom_line(data = temp, aes(x=`Local time`,y=zScorevol), color = "red")

print(p)

  class(temp)
  
start<-head(AUDCAD,2000)
end<-tail(AUDCAD,500)

View(test)

View(temp)

test<-calculateMeanOfTemporalSeries(start$Close,10)
testEma<-calculateExponentialMeanOfTemporalSeries(start$Close,10)
testWma<-calculateWeightedMeanOfTemporalSeries(start$Close,10)
standardDev<-calculateStandardDeviationOfTemporalSeries(start$Close,10)
zScore<-calculateZScoreOfTemporalSeries(start$Close,25)
plot(zScore,type="line")
plot(zScorevol,type="line")
line(zScorevol)

temp<-cbind(end,zScore,zScorevol)

plot(zScore,type="line")

standardDev[2]==0


start %>%
  mutate(Open)