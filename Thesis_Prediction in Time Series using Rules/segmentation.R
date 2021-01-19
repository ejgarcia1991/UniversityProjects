library(e1071)
library(readr)

cleanTimeFrame<-function(time){
  result<-time
  result<-substring(time,1,19)
  result<-as.POSIXct(result, format = "%d.%m.%Y %H:%M:%S",tz="UTC")-3600
  return(result)
}

vincularDataFramesSegunIndice<- function(dataset1,indexVector,dataset2){
  result<-cbind(dataset1[1,],dataset2[1,])
  for(x in 2:nrow(dataset1)){
    result<-rbind(result,cbind(dataset1[x,],dataset2[indexVector[x],]))
  }
  return(result)
}

vincularVectoresSegunIndice<- function(dataset1,indexVector,dataset2){
  result<-cbind(dataset1[1],dataset2[1])
  for(x in 1:length(dataset1)){
    result<-rbind(result,cbind(dataset1[x],dataset2[indexVector[x]]))
  }
  return(result)
}

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


for(x in pairs){
  dataH<- read_csv(paste("FOREX DATA/4-PARTITIONED/1-TRAIN/HOURLY/",x,".csv",sep=""))
  dataH<-as.data.frame(dataH)
  dataD<- read_csv(paste("FOREX DATA/4-PARTITIONED/1-TRAIN/DAILY/",x,".csv",sep=""))
  dataD<-as.data.frame(dataD)
  dataW<- read_csv(paste("FOREX DATA/4-PARTITIONED/1-TRAIN/WEEKLY/",x,".csv",sep=""))
  dataW<-as.data.frame(dataW)

  #eliminar volumen
  dataH<-cbind(dataH[,1:2],dataH[,4:ncol(dataH)])
  dataD<-cbind(dataD[,1:2],dataD[,4:ncol(dataD)])
  dataW<-cbind(dataW[,1:2],dataW[,4:ncol(dataW)])
  
  segments<-segmentar(dataH,dataD,dataW)
  
  write_csv(segments,path=paste("FOREX DATA/4-PARTITIONED/SEGMENTS/",x,"_Segments.csv",sep=""))
} #OLD



for(x in pairs){
  dataH<- read_csv(paste("FOREX DATA/3-PROCESSED/HOURLY/",x,".csv",sep=""))
  dataH<-as.data.frame(dataH)
  dataD<- read_csv(paste("FOREX DATA/3-PROCESSED/DAILY/",x,".csv",sep=""))
  dataD<-as.data.frame(dataD)
  dataW<- read_csv(paste("FOREX DATA/3-PROCESSED/WEEKLY/",x,".csv",sep=""))
  dataW<-as.data.frame(dataW)
  
  #eliminar volumen
  dataH<-cbind(dataH[,1:2],dataH[,4:ncol(dataH)])
  dataD<-cbind(dataD[,1:2],dataD[,4:ncol(dataD)])
  dataW<-cbind(dataW[,1:2],dataW[,4:ncol(dataW)])
  
  segments<-segmentar(dataH,dataD,dataW)
  
  write_csv(segments,path=paste("FOREX DATA/4-SEGMENTS/",x,"_.csv",sep=""))
}

Fuerza<-function(dataset){
  momentum<-rep(0,nrow(dataset))
  meanSD<-mean(calculateStandardDeviationOfTemporalSeries(dataset$Open,50),na.rm = TRUE)
  for(x in 2:nrow(dataset)){
    momentum[x]<-(dataset$Open[x]-dataset$Open[x-1])/meanSD
  }
  
  fuerzabase<-momentum
  
  sepSMA15<-rep(NA,nrow(dataset))
  sepSMA100<-rep(NA,nrow(dataset))
  sepSMAs<-rep(NA,nrow(dataset))
  sepEMA15<-rep(NA,nrow(dataset))
  sepEMA100<-rep(NA,nrow(dataset))
  sepEMAs<-rep(NA,nrow(dataset))
  sepWMA15<-rep(NA,nrow(dataset))
  sepWMA100<-rep(NA,nrow(dataset))
  sepWMAs<-rep(NA,nrow(dataset))
  for(x in 2:nrow(dataset)){
    sepSMA15[x]<-(dataset$Open[x]-dataset$SMA15[x])/meanSD * 0.55
    sepSMA100[x]<-(dataset$Open[x]-dataset$SMA100[x])/meanSD * 0.2
    sepSMAs[x]<-(dataset$SMA15[x]-dataset$SMA100[x])/meanSD * 0.35
    sepEMA15[x]<-(dataset$Open[x]-dataset$EMA15[x])/meanSD * 0.55
    sepEMA100[x]<-(dataset$Open[x]-dataset$EMA100[x])/meanSD * 0.2
    sepEMAs[x]<-(dataset$EMA15[x]-dataset$EMA100[x])/meanSD * 0.35
    sepWMA15[x]<-(dataset$Open[x]-dataset$WMA15[x])/meanSD * 0.55
    sepWMA100[x]<-(dataset$Open[x]-dataset$WMA100[x])/meanSD * 0.2
    sepWMAs[x]<-(dataset$WMA15[x]-dataset$WMA100[x])/meanSD * 0.35
  }
  SMAs<-(sepSMA15 + sepSMA100 + sepSMAs)/3
  EMAs<-(sepEMA15 + sepEMA100 + sepEMAs)/3
  WMAs<-(sepWMA15 + sepWMA100 + sepWMAs)/3
  
  MAs<-SMAs+EMAs+WMAs
  
  ZScore15s<-rep(NA,nrow(dataset))
  ZScore50s<-rep(NA,nrow(dataset))
  ZScores<-rep(NA,nrow(dataset))
  impZscore15<-rep(NA,nrow(dataset))
  impZscore50<-rep(NA,nrow(dataset))
  for(x in 2:nrow(dataset)){
    ZScore15s[x]<-(dataset$ZScore15[x]-dataset$SmZScore15[x])
    ZScore50s[x]<-(dataset$ZScore50[x]-dataset$SmZscore50[x])
    ZScores[x]<-(dataset$ZScore15[x]-dataset$ZScore50[x])
    impZscore15[x]<-(dataset$SmZScore15[x]-dataset$SmZScore15[x-1])
    impZscore50[x]<-(dataset$SmZscore50[x]-dataset$SmZscore50[x-1])
  }
  
  
  FZscore<-((dataset$SmZScore15+ZScore15s+dataset$SmZscore50+ZScore50s)/2+ZScores+impZscore50*1.25+impZscore15*1.4)
  
  ZScore15s<-rep(NA,nrow(dataset))
  ZScore50s<-rep(NA,nrow(dataset))
  ZScores<-rep(NA,nrow(dataset))
  impZscore15<-rep(NA,nrow(dataset))
  impZscore50<-rep(NA,nrow(dataset))
  for(x in 2:nrow(dataset)){
    ZScore15s[x]<-(dataset[x,13]-dataset[x,14])
    ZScore50s[x]<-(dataset[x,15]-dataset[x,16])
    ZScores[x]<-(dataset[x,13]-dataset[x,15])
    impZscore15[x]<-(dataset[x,14]-dataset[x-1,14])
    impZscore50[x]<-(dataset[x,16]-dataset[x-1,16])
  }
  
  FZscorepos<-((dataset[,14]+ZScore15s+dataset[,16]+ZScore50s)/2+ZScores+impZscore50*1.25+impZscore15*1.4)
  
  ZScore15s<-rep(NA,nrow(dataset))
  ZScore50s<-rep(NA,nrow(dataset))
  ZScores<-rep(NA,nrow(dataset))
  impZscore15<-rep(NA,nrow(dataset))
  impZscore50<-rep(NA,nrow(dataset))
  for(x in 2:nrow(dataset)){
    ZScore15s[x]<-(dataset[x,17]-dataset[x,18])
    ZScore50s[x]<-(dataset[x,19]-dataset[x,20])
    ZScores[x]<-(dataset[x,17]-dataset[x,19])
    impZscore15[x]<-(dataset[x,18]-dataset[x-1,18])
    impZscore50[x]<-(dataset[x,20]-dataset[x-1,20])
  }
  
  FZscoreneg<-((dataset[,18]+ZScore15s+dataset[,20]+ZScore50s)/2+ZScores+impZscore50*1.25+impZscore15*1.4)
  
  curFZscore<-(FZscorepos-FZscoreneg)
  scaledF<-scale(cbind(fuerzabase,MAs,FZscore,curFZscore))
  fuerza<-scaledF[,1]+scaledF[,2]+(scaledF[,3]+scaledF[,4])/2
  return(fuerza)
}

segmentar<-function(datasetH,datasetD,datasetW){
  fuerzaW<-Fuerza(datasetW)
  fuerzaD<-Fuerza((datasetD[,c(1,2,c(4:21))]))
  fuerzaH<-Fuerza(datasetH[,c(1,2,c(5:22))])
  
  smfuerzaW<-calculateMeanOfTemporalSeries(fuerzaW,10)
  smfuerzaD<-calculateMeanOfTemporalSeries(fuerzaD,10)
  smfuerzaH<-calculateMeanOfTemporalSeries(fuerzaH,10)
  
  smfuerzas<-vincularVectoresSegunIndice(smfuerzaD,datasetD$Week,smfuerzaW)
  smfuerzas2<-vincularVectoresSegunIndice(smfuerzaH,datasetH$Day,smfuerzas[,1])
  smfuerzas3<-vincularVectoresSegunIndice(smfuerzaH,datasetH$Day,smfuerzas[,2])
  smfuerzas4<-cbind(smfuerzas2,smfuerzas3[,2])
  
  price=datasetH$Open
  fuerza<-smfuerzas4[,1]+(smfuerzas4[,2])*0.9+smfuerzas4[,3]*0.75
  segments<-data.frame(start=1,end=1,variance=0,skewness=0,kurtosis=0,deviation=0,slope=0, Fvariance=0,Fskewness=0,Fkurtosis=0,Fdeviation=0,Fslope=0,Fstart=0,Fmean=0,Fextreme=0)
  start<-1
  end<-1
  extreme<-0
  sepPoint<-0
  accumulateddist<-0
  trend<-0
  first=TRUE
  
  for(x in 1:length(fuerza)){
    if(!is.na(fuerza[x])){
      if(first){
        start<-x
        extreme<-fuerza[x]
        first=FALSE
      }else{
        dist<-fuerza[x]-fuerza[x-1]
        if(trend==0){
          accumulateddist<-accumulateddist+dist
          trend<-ifelse(accumulateddist>0.8,1,ifelse(accumulateddist<(-0.8),-1,0))
          if(trend!=0){
            extreme<-fuerza[x]
            sepPoint<-ifelse(trend==1,extreme-accumulateddist*0.15,extreme+accumulateddist*0.15)
          }
        }else{
          if(trend==1){
            if(fuerza[x]>extreme){
              dist<-fuerza[x]-extreme
              accumulateddist<-accumulateddist+dist
              extreme<-fuerza[x]
              sepPoint<-extreme-accumulateddist*0.15
            }else{
              if(fuerza[x]<=sepPoint){
                end<-x
                
                pSlope<-(price[end]-price[start])/(end-start)
                fSlope<-(fuerza[end]-fuerza[start])/(end-start)
                
                segmdata<-data.frame(start=start,end=end,variance=var(price[start:end]),skewness=skewness(price[start:end]),kurtosis=kurtosis(price[start:end]),deviation=sd(price[start:end]),slope=pSlope, Fvariance=var(fuerza[start:end]),Fskewness=skewness(fuerza[start:end]),Fkurtosis=kurtosis(fuerza[start:end]),Fdeviation=sd(fuerza[start:end]),Fslope=fSlope,Fstart=fuerza[start],Fmean=mean(fuerza[start:end]),Fextreme=extreme)
                segments<-rbind(segments,segmdata)
                start<-x
                sepPoint<-0
                accumulateddist<-0
                trend<-0
                extreme<-fuerza[x]
                next
              }
            }
          }else{
            if(trend==-1){
              if(fuerza[x]<extreme){
                dist<-fuerza[x]-extreme
                accumulateddist<-accumulateddist+dist
                extreme<-fuerza[x]
                sepPoint<-extreme+accumulateddist*0.15
              }else{
                if(fuerza[x]>=sepPoint){
                  end<-x
                  
                  pSlope<-(price[end]-price[start])/(end-start)
                  fSlope<-(fuerza[end]-fuerza[start])/(end-start)
                  
                  segmdata<-data.frame(start=start,end=end,variance=var(price[start:end]),skewness=skewness(price[start:end]),kurtosis=kurtosis(datasetH$Open[start:end]),deviation=sd(price[start:end]),slope=pSlope, Fvariance=var(fuerza[start:end]),Fskewness=skewness(fuerza[start:end]),Fkurtosis=kurtosis(fuerza[start:end]),Fdeviation=sd(fuerza[start:end]),Fslope=fSlope,Fstart=fuerza[start],Fmean=mean(fuerza[start:end]),Fextreme=extreme)
                  segments<-rbind(segments,segmdata)
                  start<-x
                  sepPoint<-0
                  accumulateddist<-0
                  trend<-0
                  extreme<-fuerza[x]
                  next
                }
              }
            }
          }
        }
      }
    }
  }
  return(segments[-1,])
}

PrecioSegmentado<-function(spot,segments){
  st<-segments[,1]
  en<-segments[,2]
  pri<-c(spot[st[1]])
  for(x in (st[1]+1):length(spot)){
    for(y in 1:length(en)){
      if(x>=st[y] && x<=en[y]){
        slope<-(spot[en[y]]-spot[st[y]])/(en[y]-st[y])
        pri<-c(pri,spot[st[y]]+slope*(x-st[y]))
      }
    }
    if(x>en[length(en)]){
      slope<-(spot[length(spot)]-spot[en[length(en)]])/(length(spot)-en[length(en)])
      pri<-c(pri,spot[en[length(en)]]+slope*(x-en[length(en)]))
    }
  }
  return(pri)
}