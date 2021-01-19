library(readr)

currencies<-c("EUR","GBP","AUD","NZD","USD","CAD","CHF","JPY")
pairs<-rep("a",28)

count=0
for(x in 1:7){
  for(y in 2:8){
    if(y>x){
      count<-count+1
      pairs[count]<-(paste(currencies[x],currencies[y],sep=""))
    }
  }
}

#Cleans localtime to drop the GMT+1 part of the text and convert
cleanTimeFrame<-function(matrix){
  result<-matrix
  result$`Local time`<-substring(matrix$`Local time`,1,19)
  result$`Local time`<-as.POSIXct(result$`Local time`, format = "%d.%m.%Y %H:%M:%S",tz="UTC")-3600
  return(result)
}

dataH<-read_csv(paste("FOREX DATA/2-CLEANED/HOURLY/","AUDCAD",".csv",sep=""))
dataH<-as.data.frame(dataH)
dataD<-read_csv(paste("FOREX DATA/2-CLEANED/DAILY/","AUDCAD",".csv",sep=""))
dataD<-as.data.frame(dataD)
hourlyTime<-as.integer(cleanTimeFrame(dataH)[,1])
dailyTime<-as.integer(cleanTimeFrame(dataD)[,1])

dayInterval<-rep(1,length(hourlyTime))

for(x in 2:length(dailyTime)){
  nextDay<-(hourlyTime+1-dailyTime[x]>0)
  dayInterval[nextDay]<-dayInterval[nextDay]+1
}

weeklyInterval<-rep(1,length(dailyTime))
for(x in 1:630){
  weeklyTime
  pos<-x*7
  weeklyInterval[pos:length(weeklyInterval)]<-weeklyInterval[pos:length(weeklyInterval)]+1
}
weeklyInterval<-weeklyInterval[1:4406]


eur<-vector("list",4)
eur[[1]]<-0
eur[[2]]<-0
eur[[3]]<-0
eur[[4]]<-0
gbp<-eur
aud<-eur
nzd<-eur
usd<-eur
cad<-eur
chf<-eur
jpy<-eur

for(x in pairs){
  data<-read_csv(paste("FOREX DATA/2-CLEANED/HOURLY/",x,".csv",sep=""))
  data<-as.data.frame(data)
  part1<-(substring(x,1,3))
  part2<-(substring(x,4,6))
  shortZScore<-calculateZScoreOfTemporalSeries(data$Open,15)
  longZScore<-calculateZScoreOfTemporalSeries(data$Open,50)
  if(part1=="EUR"){
    eur[[1]]<-eur[[1]]+shortZScore
    eur[[3]]<-eur[[3]]+longZScore
  }
  if(part1=="GBP"){
    gbp[[1]]<-gbp[[1]]+shortZScore
    gbp[[3]]<-gbp[[3]]+longZScore
  }
  if(part1=="AUD"){
    aud[[1]]<-aud[[1]]+shortZScore
    aud[[3]]<-aud[[3]]+longZScore
  }
  if(part1=="NZD"){
    nzd[[1]]<-nzd[[1]]+shortZScore
    nzd[[3]]<-nzd[[3]]+longZScore
  }
  if(part1=="USD"){
    usd[[1]]<-usd[[1]]+shortZScore
    usd[[3]]<-usd[[3]]+longZScore
  }
  if(part1=="CAD"){
    cad[[1]]<-cad[[1]]+shortZScore
    cad[[3]]<-cad[[3]]+longZScore
  }
  if(part1=="CHF"){
    chf[[1]]<-chf[[1]]+shortZScore
    chf[[3]]<-chf[[3]]+longZScore
  }
  if(part2=="JPY"){
    jpy[[1]]<-jpy[[1]]-shortZScore
    jpy[[3]]<-jpy[[3]]-longZScore
  }
  if(part2=="GBP"){
    gbp[[1]]<-gbp[[1]]-shortZScore
    gbp[[3]]<-gbp[[3]]-longZScore
  }
  if(part2=="AUD"){
    aud[[1]]<-aud[[1]]-shortZScore
    aud[[3]]<-aud[[3]]-longZScore
  }
  if(part2=="NZD"){
    nzd[[1]]<-nzd[[1]]-shortZScore
    nzd[[3]]<-nzd[[3]]-longZScore
  }
  if(part2=="USD"){
    usd[[1]]<-usd[[1]]-shortZScore
    usd[[3]]<-usd[[3]]-longZScore
  }
  if(part2=="CAD"){
    cad[[1]]<-cad[[1]]-shortZScore
    cad[[3]]<-cad[[3]]-longZScore
  }
  if(part2=="CHF"){
    chf[[1]]<-chf[[1]]-shortZScore
    chf[[3]]<-chf[[3]]-longZScore
  }
  
}
eur[[1]]<-eur[[1]]/7
eur[[3]]<-eur[[3]]/7
gbp[[1]]<-gbp[[1]]/7
gbp[[3]]<-gbp[[3]]/7
aud[[1]]<-aud[[1]]/7
aud[[3]]<-aud[[3]]/7
nzd[[1]]<-nzd[[1]]/7
nzd[[3]]<-nzd[[3]]/7
usd[[1]]<-usd[[1]]/7
usd[[3]]<-usd[[3]]/7
cad[[1]]<-cad[[1]]/7
cad[[3]]<-cad[[3]]/7
chf[[1]]<-chf[[1]]/7
chf[[3]]<-chf[[3]]/7
jpy[[1]]<-jpy[[1]]/7
jpy[[3]]<-jpy[[3]]/7

eur[[2]]<-calculateExponentialMeanOfTemporalSeries(eur[[1]],5)
eur[[4]]<-calculateExponentialMeanOfTemporalSeries(eur[[3]],20)
gbp[[2]]<-calculateExponentialMeanOfTemporalSeries(gbp[[1]],5)
gbp[[4]]<-calculateExponentialMeanOfTemporalSeries(gbp[[3]],20)
aud[[2]]<-calculateExponentialMeanOfTemporalSeries(aud[[1]],5)
aud[[4]]<-calculateExponentialMeanOfTemporalSeries(aud[[3]],20)
nzd[[2]]<-calculateExponentialMeanOfTemporalSeries(nzd[[1]],5)
nzd[[4]]<-calculateExponentialMeanOfTemporalSeries(nzd[[3]],20)
usd[[2]]<-calculateExponentialMeanOfTemporalSeries(usd[[1]],5)
usd[[4]]<-calculateExponentialMeanOfTemporalSeries(usd[[3]],20)
cad[[2]]<-calculateExponentialMeanOfTemporalSeries(cad[[1]],5)
cad[[4]]<-calculateExponentialMeanOfTemporalSeries(cad[[3]],20)
chf[[2]]<-calculateExponentialMeanOfTemporalSeries(chf[[1]],5)
chf[[4]]<-calculateExponentialMeanOfTemporalSeries(chf[[3]],20)
jpy[[2]]<-calculateExponentialMeanOfTemporalSeries(jpy[[1]],5)
jpy[[4]]<-calculateExponentialMeanOfTemporalSeries(jpy[[3]],20)


for(x in pairs){
  data<-read_csv(paste("FOREX DATA/2-CLEANED/HOURLY/",x,".csv",sep=""))
  data<-as.data.frame(data)
  names<-c("Time","Open","Volume","Day","Week","SMA15","SMA100","EMA15","EMA100","WMA15","WMA100","ZScore15","SmZScore15","ZScore50","SmZscore50")
  ZScore<-calculateZScoreOfTemporalSeries(data$Open,15)
  ZScore2<-calculateZScoreOfTemporalSeries(data$Open,50)
  data<-cbind(data,dayInterval,ceiling((dayInterval+1)/7),calculateMeanOfTemporalSeries(data$Open,15),calculateMeanOfTemporalSeries(data$Open,100),calculateExponentialMeanOfTemporalSeries(data$Open,15),calculateExponentialMeanOfTemporalSeries(data$Open,100),calculateWMAOfTemporalSeries(data$Open,15),calculateWMAOfTemporalSeries(data$Open,100),ZScore,calculateExponentialMeanOfTemporalSeries(ZScore,5),ZScore2,calculateExponentialMeanOfTemporalSeries(ZScore2,20))
  part1<-(substring(x,1,3))
  part2<-(substring(x,4,6))
  if(part1=="EUR"){
    data<-cbind(data,eur[[1]],eur[[2]],eur[[3]],eur[[4]])
    names<-c(names,"EUR ZScore15","EUR SmZScore15","EUR ZScore50","EUR SmZscore50")
  }
  if(part1=="GBP"){
    data<-cbind(data,gbp[[1]],gbp[[2]],gbp[[3]],gbp[[4]])
    names<-c(names,"GBP ZScore15","GBP SmZScore15","GBP ZScore50","GBP SmZscore50")
  }
  if(part1=="AUD"){
    data<-cbind(data,aud[[1]],aud[[2]],aud[[3]],aud[[4]])
    names<-c(names,"AUD ZScore15","AUD SmZScore15","AUD ZScore50","AUD SmZscore50")
  }
  if(part1=="NZD"){
    data<-cbind(data,nzd[[1]],nzd[[2]],nzd[[3]],nzd[[4]])
    names<-c(names,"NZD ZScore15","NZD SmZScore15","NZD ZScore50","NZD SmZscore50")
  }
  if(part1=="USD"){
    data<-cbind(data,usd[[1]],usd[[2]],usd[[3]],usd[[4]])
    names<-c(names,"USD ZScore15","USD SmZScore15","USD ZScore50","USD SmZscore50")
  }
  if(part1=="CAD"){
    data<-cbind(data,cad[[1]],cad[[2]],cad[[3]],cad[[4]])
    names<-c(names,"CAD ZScore15","CAD SmZScore15","CAD ZScore50","CAD SmZscore50")
  }
  if(part1=="CHF"){
    data<-cbind(data,chf[[1]],chf[[2]],chf[[3]],chf[[4]])
    names<-c(names,"CHF ZScore15","CHF SmZScore15","CHF ZScore50","CHF SmZscore50")
  }
  if(part2=="JPY"){
    data<-cbind(data,jpy[[1]],jpy[[2]],jpy[[3]],jpy[[4]])
    names<-c(names,"JPY ZScore15","JPY SmZScore15","JPY ZScore50","JPY SmZscore50")
  }
  if(part2=="GBP"){
    data<-cbind(data,gbp[[1]],gbp[[2]],gbp[[3]],gbp[[4]])
    names<-c(names,"GBP ZScore15","GBP SmZScore15","GBP ZScore50","GBP SmZscore50")
  }
  if(part2=="AUD"){
    data<-cbind(data,aud[[1]],aud[[2]],aud[[3]],aud[[4]])
    names<-c(names,"AUD ZScore15","AUD SmZScore15","AUD ZScore50","AUD SmZscore50")
  }
  if(part2=="NZD"){
    data<-cbind(data,nzd[[1]],nzd[[2]],nzd[[3]],nzd[[4]])
    names<-c(names,"NZD ZScore15","NZD SmZScore15","NZD ZScore50","NZD SmZscore50")
  }
  if(part2=="USD"){
    data<-cbind(data,usd[[1]],usd[[2]],usd[[3]],usd[[4]])
    names<-c(names,"USD ZScore15","USD SmZScore15","USD ZScore50","USD SmZscore50")
  }
  if(part2=="CAD"){
    data<-cbind(data,cad[[1]],cad[[2]],cad[[3]],cad[[4]])
    names<-c(names,"CAD ZScore15","CAD SmZScore15","CAD ZScore50","CAD SmZscore50")
  }
  if(part2=="CHF"){
    data<-cbind(data,chf[[1]],chf[[2]],chf[[3]],chf[[4]])
    names<-c(names,"CHF ZScore15","CHF SmZScore15","CHF ZScore50","CHF SmZscore50")
  }
  colnames(data)<-names
  write_csv(data,path=paste("FOREX DATA/3-PROCESSED/HOURLY/",x,".csv",sep=""))
}


eur<-vector("list",4)
eur[[1]]<-0
eur[[2]]<-0
eur[[3]]<-0
eur[[4]]<-0
gbp<-eur
aud<-eur
nzd<-eur
usd<-eur
cad<-eur
chf<-eur
jpy<-eur

for(x in pairs){
  data<-read_csv(paste("FOREX DATA/2-CLEANED/DAILY/",x,".csv",sep=""))
  data<-as.data.frame(data)
  part1<-(substring(x,1,3))
  part2<-(substring(x,4,6))
  shortZScore<-calculateZScoreOfTemporalSeries(data$Open,15)
  longZScore<-calculateZScoreOfTemporalSeries(data$Open,50)
  if(part1=="EUR"){
    eur[[1]]<-eur[[1]]+shortZScore
    eur[[3]]<-eur[[3]]+longZScore
  }
  if(part1=="GBP"){
    gbp[[1]]<-gbp[[1]]+shortZScore
    gbp[[3]]<-gbp[[3]]+longZScore
  }
  if(part1=="AUD"){
    aud[[1]]<-aud[[1]]+shortZScore
    aud[[3]]<-aud[[3]]+longZScore
  }
  if(part1=="NZD"){
    nzd[[1]]<-nzd[[1]]+shortZScore
    nzd[[3]]<-nzd[[3]]+longZScore
  }
  if(part1=="USD"){
    usd[[1]]<-usd[[1]]+shortZScore
    usd[[3]]<-usd[[3]]+longZScore
  }
  if(part1=="CAD"){
    cad[[1]]<-cad[[1]]+shortZScore
    cad[[3]]<-cad[[3]]+longZScore
  }
  if(part1=="CHF"){
    chf[[1]]<-chf[[1]]+shortZScore
    chf[[3]]<-chf[[3]]+longZScore
  }
  if(part2=="JPY"){
    jpy[[1]]<-jpy[[1]]-shortZScore
    jpy[[3]]<-jpy[[3]]-longZScore
  }
  if(part2=="GBP"){
    gbp[[1]]<-gbp[[1]]-shortZScore
    gbp[[3]]<-gbp[[3]]-longZScore
  }
  if(part2=="AUD"){
    aud[[1]]<-aud[[1]]-shortZScore
    aud[[3]]<-aud[[3]]-longZScore
  }
  if(part2=="NZD"){
    nzd[[1]]<-nzd[[1]]-shortZScore
    nzd[[3]]<-nzd[[3]]-longZScore
  }
  if(part2=="USD"){
    usd[[1]]<-usd[[1]]-shortZScore
    usd[[3]]<-usd[[3]]-longZScore
  }
  if(part2=="CAD"){
    cad[[1]]<-cad[[1]]-shortZScore
    cad[[3]]<-cad[[3]]-longZScore
  }
  if(part2=="CHF"){
    chf[[1]]<-chf[[1]]-shortZScore
    chf[[3]]<-chf[[3]]-longZScore
  }
  
}
eur[[1]]<-eur[[1]]/7
eur[[3]]<-eur[[3]]/7
gbp[[1]]<-gbp[[1]]/7
gbp[[3]]<-gbp[[3]]/7
aud[[1]]<-aud[[1]]/7
aud[[3]]<-aud[[3]]/7
nzd[[1]]<-nzd[[1]]/7
nzd[[3]]<-nzd[[3]]/7
usd[[1]]<-usd[[1]]/7
usd[[3]]<-usd[[3]]/7
cad[[1]]<-cad[[1]]/7
cad[[3]]<-cad[[3]]/7
chf[[1]]<-chf[[1]]/7
chf[[3]]<-chf[[3]]/7
jpy[[1]]<-jpy[[1]]/7
jpy[[3]]<-jpy[[3]]/7

eur[[2]]<-calculateExponentialMeanOfTemporalSeries(eur[[1]],5)
eur[[4]]<-calculateExponentialMeanOfTemporalSeries(eur[[3]],20)
gbp[[2]]<-calculateExponentialMeanOfTemporalSeries(gbp[[1]],5)
gbp[[4]]<-calculateExponentialMeanOfTemporalSeries(gbp[[3]],20)
aud[[2]]<-calculateExponentialMeanOfTemporalSeries(aud[[1]],5)
aud[[4]]<-calculateExponentialMeanOfTemporalSeries(aud[[3]],20)
nzd[[2]]<-calculateExponentialMeanOfTemporalSeries(nzd[[1]],5)
nzd[[4]]<-calculateExponentialMeanOfTemporalSeries(nzd[[3]],20)
usd[[2]]<-calculateExponentialMeanOfTemporalSeries(usd[[1]],5)
usd[[4]]<-calculateExponentialMeanOfTemporalSeries(usd[[3]],20)
cad[[2]]<-calculateExponentialMeanOfTemporalSeries(cad[[1]],5)
cad[[4]]<-calculateExponentialMeanOfTemporalSeries(cad[[3]],20)
chf[[2]]<-calculateExponentialMeanOfTemporalSeries(chf[[1]],5)
chf[[4]]<-calculateExponentialMeanOfTemporalSeries(chf[[3]],20)
jpy[[2]]<-calculateExponentialMeanOfTemporalSeries(jpy[[1]],5)
jpy[[4]]<-calculateExponentialMeanOfTemporalSeries(jpy[[3]],20)

for(x in pairs){
  data<-read_csv(paste("FOREX DATA/2-CLEANED/DAILY/",x,".csv",sep=""))
  data<-as.data.frame(data)
  names<-c("Time","Open","Volume","Week","SMA15","SMA100","EMA15","EMA100","WMA15","WMA100","ZScore15","SmZScore15","ZScore50","SmZscore50")
  ZScore<-calculateZScoreOfTemporalSeries(data$Open,15)
  ZScore2<-calculateZScoreOfTemporalSeries(data$Open,50)
  data<-cbind(data,weeklyInterval,calculateMeanOfTemporalSeries(data$Open,15),calculateMeanOfTemporalSeries(data$Open,100),calculateExponentialMeanOfTemporalSeries(data$Open,15),calculateExponentialMeanOfTemporalSeries(data$Open,100),calculateWMAOfTemporalSeries(data$Open,15),calculateWMAOfTemporalSeries(data$Open,100),ZScore,calculateExponentialMeanOfTemporalSeries(ZScore,5),ZScore2,calculateExponentialMeanOfTemporalSeries(ZScore2,20))
  part1<-(substring(x,1,3))
  part2<-(substring(x,4,6))
  if(part1=="EUR"){
    data<-cbind(data,eur[[1]],eur[[2]],eur[[3]],eur[[4]])
    names<-c(names,"EUR ZScore15","EUR SmZScore15","EUR ZScore50","EUR SmZscore50")
  }
  if(part1=="GBP"){
    data<-cbind(data,gbp[[1]],gbp[[2]],gbp[[3]],gbp[[4]])
    names<-c(names,"GBP ZScore15","GBP SmZScore15","GBP ZScore50","GBP SmZscore50")
  }
  if(part1=="AUD"){
    data<-cbind(data,aud[[1]],aud[[2]],aud[[3]],aud[[4]])
    names<-c(names,"AUD ZScore15","AUD SmZScore15","AUD ZScore50","AUD SmZscore50")
  }
  if(part1=="NZD"){
    data<-cbind(data,nzd[[1]],nzd[[2]],nzd[[3]],nzd[[4]])
    names<-c(names,"NZD ZScore15","NZD SmZScore15","NZD ZScore50","NZD SmZscore50")
  }
  if(part1=="USD"){
    data<-cbind(data,usd[[1]],usd[[2]],usd[[3]],usd[[4]])
    names<-c(names,"USD ZScore15","USD SmZScore15","USD ZScore50","USD SmZscore50")
  }
  if(part1=="CAD"){
    data<-cbind(data,cad[[1]],cad[[2]],cad[[3]],cad[[4]])
    names<-c(names,"CAD ZScore15","CAD SmZScore15","CAD ZScore50","CAD SmZscore50")
  }
  if(part1=="CHF"){
    data<-cbind(data,chf[[1]],chf[[2]],chf[[3]],chf[[4]])
    names<-c(names,"CHF ZScore15","CHF SmZScore15","CHF ZScore50","CHF SmZscore50")
  }
  if(part2=="JPY"){
    data<-cbind(data,jpy[[1]],jpy[[2]],jpy[[3]],jpy[[4]])
    names<-c(names,"JPY ZScore15","JPY SmZScore15","JPY ZScore50","JPY SmZscore50")
  }
  if(part2=="GBP"){
    data<-cbind(data,gbp[[1]],gbp[[2]],gbp[[3]],gbp[[4]])
    names<-c(names,"GBP ZScore15","GBP SmZScore15","GBP ZScore50","GBP SmZscore50")
  }
  if(part2=="AUD"){
    data<-cbind(data,aud[[1]],aud[[2]],aud[[3]],aud[[4]])
    names<-c(names,"AUD ZScore15","AUD SmZScore15","AUD ZScore50","AUD SmZscore50")
  }
  if(part2=="NZD"){
    data<-cbind(data,nzd[[1]],nzd[[2]],nzd[[3]],nzd[[4]])
    names<-c(names,"NZD ZScore15","NZD SmZScore15","NZD ZScore50","NZD SmZscore50")
  }
  if(part2=="USD"){
    data<-cbind(data,usd[[1]],usd[[2]],usd[[3]],usd[[4]])
    names<-c(names,"USD ZScore15","USD SmZScore15","USD ZScore50","USD SmZscore50")
  }
  if(part2=="CAD"){
    data<-cbind(data,cad[[1]],cad[[2]],cad[[3]],cad[[4]])
    names<-c(names,"CAD ZScore15","CAD SmZScore15","CAD ZScore50","CAD SmZscore50")
  }
  if(part2=="CHF"){
    data<-cbind(data,chf[[1]],chf[[2]],chf[[3]],chf[[4]])
    names<-c(names,"CHF ZScore15","CHF SmZScore15","CHF ZScore50","CHF SmZscore50")
  }
  colnames(data)<-names
  write_csv(data,path=paste("FOREX DATA/3-PROCESSED/DAILY/",x,".csv",sep=""))
}


eur<-vector("list",4)
eur[[1]]<-0
eur[[2]]<-0
eur[[3]]<-0
eur[[4]]<-0
gbp<-eur
aud<-eur
nzd<-eur
usd<-eur
cad<-eur
chf<-eur
jpy<-eur

for(x in pairs){
  data<-read_csv(paste("FOREX DATA/2-CLEANED/WEEKLY/",x,".csv",sep=""))
  data<-as.data.frame(data)
  part1<-(substring(x,1,3))
  part2<-(substring(x,4,6))
  shortZScore<-calculateZScoreOfTemporalSeries(data$Open,15)
  longZScore<-calculateZScoreOfTemporalSeries(data$Open,50)
  if(part1=="EUR"){
    eur[[1]]<-eur[[1]]+shortZScore
    eur[[3]]<-eur[[3]]+longZScore
  }
  if(part1=="GBP"){
    gbp[[1]]<-gbp[[1]]+shortZScore
    gbp[[3]]<-gbp[[3]]+longZScore
  }
  if(part1=="AUD"){
    aud[[1]]<-aud[[1]]+shortZScore
    aud[[3]]<-aud[[3]]+longZScore
  }
  if(part1=="NZD"){
    nzd[[1]]<-nzd[[1]]+shortZScore
    nzd[[3]]<-nzd[[3]]+longZScore
  }
  if(part1=="USD"){
    usd[[1]]<-usd[[1]]+shortZScore
    usd[[3]]<-usd[[3]]+longZScore
  }
  if(part1=="CAD"){
    cad[[1]]<-cad[[1]]+shortZScore
    cad[[3]]<-cad[[3]]+longZScore
  }
  if(part1=="CHF"){
    chf[[1]]<-chf[[1]]+shortZScore
    chf[[3]]<-chf[[3]]+longZScore
  }
  if(part2=="JPY"){
    jpy[[1]]<-jpy[[1]]-shortZScore
    jpy[[3]]<-jpy[[3]]-longZScore
  }
  if(part2=="GBP"){
    gbp[[1]]<-gbp[[1]]-shortZScore
    gbp[[3]]<-gbp[[3]]-longZScore
  }
  if(part2=="AUD"){
    aud[[1]]<-aud[[1]]-shortZScore
    aud[[3]]<-aud[[3]]-longZScore
  }
  if(part2=="NZD"){
    nzd[[1]]<-nzd[[1]]-shortZScore
    nzd[[3]]<-nzd[[3]]-longZScore
  }
  if(part2=="USD"){
    usd[[1]]<-usd[[1]]-shortZScore
    usd[[3]]<-usd[[3]]-longZScore
  }
  if(part2=="CAD"){
    cad[[1]]<-cad[[1]]-shortZScore
    cad[[3]]<-cad[[3]]-longZScore
  }
  if(part2=="CHF"){
    chf[[1]]<-chf[[1]]-shortZScore
    chf[[3]]<-chf[[3]]-longZScore
  }
  
}
eur[[1]]<-eur[[1]]/7
eur[[3]]<-eur[[3]]/7
gbp[[1]]<-gbp[[1]]/7
gbp[[3]]<-gbp[[3]]/7
aud[[1]]<-aud[[1]]/7
aud[[3]]<-aud[[3]]/7
nzd[[1]]<-nzd[[1]]/7
nzd[[3]]<-nzd[[3]]/7
usd[[1]]<-usd[[1]]/7
usd[[3]]<-usd[[3]]/7
cad[[1]]<-cad[[1]]/7
cad[[3]]<-cad[[3]]/7
chf[[1]]<-chf[[1]]/7
chf[[3]]<-chf[[3]]/7
jpy[[1]]<-jpy[[1]]/7
jpy[[3]]<-jpy[[3]]/7

eur[[2]]<-calculateExponentialMeanOfTemporalSeries(eur[[1]],5)
eur[[4]]<-calculateExponentialMeanOfTemporalSeries(eur[[3]],20)
gbp[[2]]<-calculateExponentialMeanOfTemporalSeries(gbp[[1]],5)
gbp[[4]]<-calculateExponentialMeanOfTemporalSeries(gbp[[3]],20)
aud[[2]]<-calculateExponentialMeanOfTemporalSeries(aud[[1]],5)
aud[[4]]<-calculateExponentialMeanOfTemporalSeries(aud[[3]],20)
nzd[[2]]<-calculateExponentialMeanOfTemporalSeries(nzd[[1]],5)
nzd[[4]]<-calculateExponentialMeanOfTemporalSeries(nzd[[3]],20)
usd[[2]]<-calculateExponentialMeanOfTemporalSeries(usd[[1]],5)
usd[[4]]<-calculateExponentialMeanOfTemporalSeries(usd[[3]],20)
cad[[2]]<-calculateExponentialMeanOfTemporalSeries(cad[[1]],5)
cad[[4]]<-calculateExponentialMeanOfTemporalSeries(cad[[3]],20)
chf[[2]]<-calculateExponentialMeanOfTemporalSeries(chf[[1]],5)
chf[[4]]<-calculateExponentialMeanOfTemporalSeries(chf[[3]],20)
jpy[[2]]<-calculateExponentialMeanOfTemporalSeries(jpy[[1]],5)
jpy[[4]]<-calculateExponentialMeanOfTemporalSeries(jpy[[3]],20)

for(x in pairs){
  data<-read_csv(paste("FOREX DATA/2-CLEANED/WEEKLY/",x,".csv",sep=""))
  data<-as.data.frame(data)
  names<-c("Time","Open","Volume","SMA15","SMA100","EMA15","EMA100","WMA15","WMA100","ZScore15","SmZScore15","ZScore50","SmZscore50")
  ZScore<-calculateZScoreOfTemporalSeries(data$Open,15)
  ZScore2<-calculateZScoreOfTemporalSeries(data$Open,50)
  data<-cbind(data,calculateMeanOfTemporalSeries(data$Open,15),calculateMeanOfTemporalSeries(data$Open,100),calculateExponentialMeanOfTemporalSeries(data$Open,15),calculateExponentialMeanOfTemporalSeries(data$Open,100),calculateWMAOfTemporalSeries(data$Open,15),calculateWMAOfTemporalSeries(data$Open,100),ZScore,calculateExponentialMeanOfTemporalSeries(ZScore,5),ZScore2,calculateExponentialMeanOfTemporalSeries(ZScore2,20))
  part1<-(substring(x,1,3))
  part2<-(substring(x,4,6))
  if(part1=="EUR"){
    data<-cbind(data,eur[[1]],eur[[2]],eur[[3]],eur[[4]])
    names<-c(names,"EUR ZScore15","EUR SmZScore15","EUR ZScore50","EUR SmZscore50")
  }
  if(part1=="GBP"){
    data<-cbind(data,gbp[[1]],gbp[[2]],gbp[[3]],gbp[[4]])
    names<-c(names,"GBP ZScore15","GBP SmZScore15","GBP ZScore50","GBP SmZscore50")
  }
  if(part1=="AUD"){
    data<-cbind(data,aud[[1]],aud[[2]],aud[[3]],aud[[4]])
    names<-c(names,"AUD ZScore15","AUD SmZScore15","AUD ZScore50","AUD SmZscore50")
  }
  if(part1=="NZD"){
    data<-cbind(data,nzd[[1]],nzd[[2]],nzd[[3]],nzd[[4]])
    names<-c(names,"NZD ZScore15","NZD SmZScore15","NZD ZScore50","NZD SmZscore50")
  }
  if(part1=="USD"){
    data<-cbind(data,usd[[1]],usd[[2]],usd[[3]],usd[[4]])
    names<-c(names,"USD ZScore15","USD SmZScore15","USD ZScore50","USD SmZscore50")
  }
  if(part1=="CAD"){
    data<-cbind(data,cad[[1]],cad[[2]],cad[[3]],cad[[4]])
    names<-c(names,"CAD ZScore15","CAD SmZScore15","CAD ZScore50","CAD SmZscore50")
  }
  if(part1=="CHF"){
    data<-cbind(data,chf[[1]],chf[[2]],chf[[3]],chf[[4]])
    names<-c(names,"CHF ZScore15","CHF SmZScore15","CHF ZScore50","CHF SmZscore50")
  }
  if(part2=="JPY"){
    data<-cbind(data,jpy[[1]],jpy[[2]],jpy[[3]],jpy[[4]])
    names<-c(names,"JPY ZScore15","JPY SmZScore15","JPY ZScore50","JPY SmZscore50")
  }
  if(part2=="GBP"){
    data<-cbind(data,gbp[[1]],gbp[[2]],gbp[[3]],gbp[[4]])
    names<-c(names,"GBP ZScore15","GBP SmZScore15","GBP ZScore50","GBP SmZscore50")
  }
  if(part2=="AUD"){
    data<-cbind(data,aud[[1]],aud[[2]],aud[[3]],aud[[4]])
    names<-c(names,"AUD ZScore15","AUD SmZScore15","AUD ZScore50","AUD SmZscore50")
  }
  if(part2=="NZD"){
    data<-cbind(data,nzd[[1]],nzd[[2]],nzd[[3]],nzd[[4]])
    names<-c(names,"NZD ZScore15","NZD SmZScore15","NZD ZScore50","NZD SmZscore50")
  }
  if(part2=="USD"){
    data<-cbind(data,usd[[1]],usd[[2]],usd[[3]],usd[[4]])
    names<-c(names,"USD ZScore15","USD SmZScore15","USD ZScore50","USD SmZscore50")
  }
  if(part2=="CAD"){
    data<-cbind(data,cad[[1]],cad[[2]],cad[[3]],cad[[4]])
    names<-c(names,"CAD ZScore15","CAD SmZScore15","CAD ZScore50","CAD SmZscore50")
  }
  if(part2=="CHF"){
    data<-cbind(data,chf[[1]],chf[[2]],chf[[3]],chf[[4]])
    names<-c(names,"CHF ZScore15","CHF SmZScore15","CHF ZScore50","CHF SmZscore50")
  }
  colnames(data)<-names
  write_csv(data,path=paste("FOREX DATA/3-PROCESSED/WEEKLY/",x,".csv",sep=""))
}

cbind(AUDCAD[0,],AUDUSD[0,])




for(x in pairs){
  data<-read_csv(paste("FOREX DATA/3-PROCESSED/WEEKLY/",x,".csv",sep=""))
  data<-as.data.frame(data)
  train_index<-round(nrow(data)/100*70) #TRAIN
  test_index<-round(nrow(data)/100*85) #TEST
  holdout_index<-nrow(data) #HOLDOUT
  train<-data[1:train_index,]
  test<-data[(train_index+1):test_index,]
  holdout<-data[(test_index+1):holdout_index,]

write_csv(train,path=paste("FOREX DATA/4-PARTITIONED/1-TRAIN/WEEKLY/",x,".csv",sep=""))
write_csv(test,path=paste("FOREX DATA/4-PARTITIONED/2-TEST/WEEKLY/",x,".csv",sep=""))
write_csv(holdout,path=paste("FOREX DATA/4-PARTITIONED/3-FINAL TEST/WEEKLY/",x,".csv",sep=""))
}


Ddata<-read_csv(paste("FOREX DATA/2-CLEANED/DAILY/","AUDCAD",".csv",sep=""))
Ddata<-as.data.frame(Ddata)

Ddata<-Ddata[,c(1,2,7)]
colnames(Ddata)[3]<-"interval"



for(x in pairs){
  data<-read_csv(paste("FOREX DATA/1-ORIGINAL/DAILY/",x,"_Candlestick_1_D_BID_01.01.2008-25.01.2020.csv",sep=""))
  data<-as.data.frame(data)
  cleanData<-cleanNonTradedDays(data)
  write_csv(cleanData,path=paste("FOREX DATA/2-CLEANED/DAILY/",x,".csv",sep=""))
}


for(x in pairs){
  data<-read_csv(paste("FOREX DATA/1-ORIGINAL/WEEKLY/",x,"_Candlestick_1_W_BID_01.01.2008-25.01.2020.csv",sep=""))
  data<-as.data.frame(data)
  write_csv(cleanData,path=paste("FOREX DATA/2-CLEANED/WEEKLY/",x,".csv",sep=""))
}

#La información de los dos puntos anteriores, pero de los intervalos superiores. Dígase la SMA, EMA, WMA del par del día y de la semana, así como el Z-Score de las divisas y una media móvil de las mismas del par del día y de la semana. Esta información podría ser resumida en variables categóricas si el modelo a utilizar obtiene mayor precisión de esta forma.