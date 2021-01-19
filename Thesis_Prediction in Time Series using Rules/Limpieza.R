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

validIndexes<-rep(FALSE,105792)
for(y in pairs){
  data<-read_csv(paste("FOREX DATA/1-ORIGINAL/HOURLY/",y,"_Candlestick_1_Hour_BID_01.01.2008-25.01.2020.csv",sep=""))
  data<-as.data.frame(data)
  for(x in 2:105792){
    
    if(data$Open[x]!=data$Open[x-1] || data$Volume[x]!=0){
      validIndexes[x]<-TRUE
    }
  }
  
}

for(x in pairs){
  data<-read_csv(paste("FOREX DATA/1-ORIGINAL/HOURLY/",x,"_Candlestick_1_Hour_BID_01.01.2008-25.01.2020.csv",sep=""))
  data<-as.data.frame(data)
  data<-data[,c(1,2,6)]
  write_csv(data[validIndexes,],path=paste("FOREX DATA/2-CLEANED/HOURLY/",x,".csv",sep=""))
}

validIndexes<-rep(FALSE,4409)
for(y in pairs){
  data<-read_csv(paste("FOREX DATA/1-ORIGINAL/DAILY/",y,"_Candlestick_1_D_BID_01.01.2008-25.01.2020.csv",sep=""))
  data<-as.data.frame(data)
  for(x in 2:4409){
    
    if(data$Open[x]!=data$Open[x-1] || data$Volume[x]!=0){
      validIndexes[x]<-TRUE
    }
  }
}

for(x in pairs){
  data<-read_csv(paste("FOREX DATA/1-ORIGINAL/DAILY/",x,"_Candlestick_1_D_BID_01.01.2008-25.01.2020.csv",sep=""))
  data<-as.data.frame(data)
  data<-data[,c(1,2,6)]
  write_csv(data[validIndexes,],path=paste("FOREX DATA/2-CLEANED/DAILY/",x,".csv",sep=""))
}


for(x in pairs){
  data<-read_csv(paste("FOREX DATA/1-ORIGINAL/WEEKLY/",x,"_Candlestick_1_W_BID_01.01.2008-25.01.2020.csv",sep=""))
  data<-as.data.frame(data)
  data<-data[,c(1,2,6)]
  write_csv(data,path=paste("FOREX DATA/2-CLEANED/WEEKLY/",x,".csv",sep=""))
}