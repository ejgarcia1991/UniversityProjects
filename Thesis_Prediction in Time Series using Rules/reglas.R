df<-read.csv("test.csv",header=FALSE)
prepareData(df,3)

price_cluster<-read.csv(paste("FOREX DATA/4-PARTITIONED/CLUSTERS/","AUDCAD","_PriceCluster_5_Pearson.csv",sep=""))
force_cluster<-read.csv(paste("FOREX DATA/4-PARTITIONED/CLUSTERS/","EURGBP","_ForceCluster_5_Pearson.csv",sep=""))


for(y in c("Supervised3","Supervised3_15","Supervised3_20","Slope3","SlopeVar3","Force5","Price5")){
  for(x in pairs){
    data<- read_csv(paste("FOREX DATA/5-CLUSTERS/",y,"/",x,"_Ord.csv",sep=""))
    data<-as.data.frame(data)
    train_index<-round(nrow(data)/100*80) #TRAIN
    train<-data[1:train_index,]
    test<-data[(train_index-19):nrow(data),]
    
    write_csv(train,path=paste("FOREX DATA/6-TRAIN/",y,"/",x,".csv",sep=""))
    write_csv(test,path=paste("FOREX DATA/8-TEST/",y,"/",x,".csv",sep=""))
  }
} # separation into Train/Test



for(x in pairs){
  data<- read_csv(paste("FOREX DATA/5-CLUSTERS/Supervised3/",x,"_Ord.csv",sep=""))
  data<-as.data.frame(data)
  train_index<-round(nrow(data)/100*80) #TRAIN
  train<-data[1:train_index,]
  test<-data[(train_index-19):nrow(data),]
  
  write_csv(train,path=paste("FOREX DATA/6-TRAIN/Supervised3/",x,".csv",sep=""))
  write_csv(test,path=paste("FOREX DATA/8-TEST/Supervised3/",x,".csv",sep=""))
  
  data<- read_csv(paste("FOREX DATA/5-CLUSTERS/Supervised3_15/",x,"_Ord.csv",sep=""))
  data<-as.data.frame(data)
  train_index<-round(nrow(data)/100*80) #TRAIN
  train<-data[1:train_index,]
  test<-data[(train_index-19):nrow(data),]
  
  write_csv(train,path=paste("FOREX DATA/6-TRAIN/Supervised3_15/",x,".csv",sep=""))
  write_csv(test,path=paste("FOREX DATA/8-TEST/Supervised3_15/",x,".csv",sep=""))
  
  data<- read_csv(paste("FOREX DATA/5-CLUSTERS/Supervised3/",x,"_Ord.csv",sep=""))
  data<-as.data.frame(data)
  train_index<-round(nrow(data)/100*80) #TRAIN
  train<-data[1:train_index,]
  test<-data[(train_index-19):nrow(data),]
  
  write_csv(train,path=paste("FOREX DATA/6-TRAIN/Supervised3/",x,".csv",sep=""))
  write_csv(test,path=paste("FOREX DATA/8-TEST/Supervised3/",x,".csv",sep=""))
  
  data<- read_csv(paste("FOREX DATA/5-CLUSTERS/SlopeVar3/",x,"_Ord.csv",sep=""))
  data<-as.data.frame(data)
  train_index<-round(nrow(data)/100*80) #TRAIN
  train<-data[1:train_index,]
  test<-data[(train_index-19):nrow(data),]
  
  write_csv(train,path=paste("FOREX DATA/6-TRAIN/SlopeVar3/",x,".csv",sep=""))
  write_csv(test,path=paste("FOREX DATA/8-TEST/SlopeVar3/",x,".csv",sep=""))
  
  data<- read_csv(paste("FOREX DATA/5-CLUSTERS/Slope3/",x,"_Ord.csv",sep=""))
  data<-as.data.frame(data)
  train_index<-round(nrow(data)/100*80) #TRAIN
  train<-data[1:train_index,]
  test<-data[(train_index-19):nrow(data),]
  
  write_csv(train,path=paste("FOREX DATA/6-TRAIN/Slope3/",x,".csv",sep=""))
  write_csv(test,path=paste("FOREX DATA/8-TEST/Slope3/",x,".csv",sep=""))
  
  data<- read_csv(paste("FOREX DATA/5-CLUSTERS/Force5/",x,"_Ord.csv",sep=""))
  data<-as.data.frame(data)
  train_index<-round(nrow(data)/100*80) #TRAIN
  train<-data[1:train_index,]
  test<-data[(train_index-19):nrow(data),]
  
  write_csv(train,path=paste("FOREX DATA/6-TRAIN/Force5/",x,".csv",sep=""))
  write_csv(test,path=paste("FOREX DATA/8-TEST/Force5/",x,".csv",sep=""))
  
  data<- read_csv(paste("FOREX DATA/5-CLUSTERS/Price5/",x,"_Ord.csv",sep=""))
  data<-as.data.frame(data)
  train_index<-round(nrow(data)/100*80) #TRAIN
  train<-data[1:train_index,]
  test<-data[(train_index-19):nrow(data),]
  
  write_csv(train,path=paste("FOREX DATA/6-TRAIN/Price5/",x,".csv",sep=""))
  write_csv(test,path=paste("FOREX DATA/8-TEST/Price5/",x,".csv",sep=""))
} # separation into Train/Test OLD

for(y in c("SupervisedBinPrecio")){
  for(x in pairs){
    data<- read_csv(paste("FOREX DATA/5-CLUSTERS/",y,"/",x,"_Ord.csv",sep=""))
    data<-as.data.frame(data)
    train_index<-round(nrow(data)/100*80) #TRAIN
    train<-data[1:train_index,]
    test<-data[(train_index-19):nrow(data),]
    
    write_csv(train,path=paste("FOREX DATA/6-TRAIN/",y,"/",x,".csv",sep=""))
    write_csv(test,path=paste("FOREX DATA/8-TEST/",y,"/",x,".csv",sep=""))
  }
} #1

for(y in c("SupervisedBinPrecio")){
  dataf=as.data.frame(rbind(cbind("/","/",0)))
  for(x in 1:28){
    cluster<- read_csv(paste("FOREX DATA/6-TRAIN/",y,"/",pairs[x],".csv",sep=""))
    df<-cluster[,16]
    df<-as.data.frame(df)
    dataf<-prepareData(df,14,dataf)
  }
  dataf<-extendData(dataf)
  write_csv(dataf,path=paste("FOREX DATA/7-RULES/",y,"/","ALL",".csv",sep=""))
  
  dataf=as.data.frame(rbind(cbind("/","/",0)))
  cluster<- read_csv(paste("FOREX DATA/6-TRAIN/",y,"/","EURNZD",".csv",sep=""))
  df<-cluster[,16]
  df<-as.data.frame(df)
  dataf<-prepareData(df,14,dataf)
  dataf<-extendData(dataf)
  write_csv(dataf,path=paste("FOREX DATA/7-RULES/",y,"/","EURNZD",".csv",sep=""))
  
  dataf=as.data.frame(rbind(cbind("/","/",0)))
  cluster<- read_csv(paste("FOREX DATA/6-TRAIN/",y,"/","AUDCAD",".csv",sep=""))
  df<-cluster[,16]
  df<-as.data.frame(df)
  dataf<-prepareData(df,14,dataf)
  dataf<-extendData(dataf)
  write_csv(dataf,path=paste("FOREX DATA/7-RULES/",y,"/","AUDCAD",".csv",sep=""))
} #2


for(y in c("Price5","Force5")){
dataf=as.data.frame(rbind(cbind("/","/",0)))
for(x in 1:28){
  cluster<- read_csv(paste("FOREX DATA/6-TRAIN/",y,"/",pairs[x],".csv",sep=""))
  df<-cluster[,16]
  df<-as.data.frame(df)
  dataf<-prepareData(df,6,dataf)
}
dataf<-extendData(dataf)
write_csv(dataf,path=paste("FOREX DATA/7-RULES/",y,"/","ALL",".csv",sep=""))

dataf=as.data.frame(rbind(cbind("/","/",0)))
cluster<- read_csv(paste("FOREX DATA/6-TRAIN/",y,"/","EURNZD",".csv",sep=""))
df<-cluster[,16]
df<-as.data.frame(df)
dataf<-prepareData(df,6,dataf)
dataf<-extendData(dataf)
write_csv(dataf,path=paste("FOREX DATA/7-RULES/",y,"/","EURNZD",".csv",sep=""))

dataf=as.data.frame(rbind(cbind("/","/",0)))
cluster<- read_csv(paste("FOREX DATA/6-TRAIN/",y,"/","AUDCAD",".csv",sep=""))
df<-cluster[,16]
df<-as.data.frame(df)
dataf<-prepareData(df,6,dataf)
dataf<-extendData(dataf)
write_csv(dataf,path=paste("FOREX DATA/7-RULES/",y,"/","AUDCAD",".csv",sep=""))
}

for(y in c("Slope3","SlopeVar3","Supervised3", "Supervised3_15", "Supervised3_20")){
  dataf=as.data.frame(rbind(cbind("/","/",0)))
  for(x in 1:28){
    cluster<- read_csv(paste("FOREX DATA/6-TRAIN/",y,"/",pairs[x],".csv",sep=""))
    df<-cluster[,16]
    df<-as.data.frame(df)
    dataf<-prepareData(df,8,dataf)
  }
  dataf<-extendData(dataf)
  write_csv(dataf,path=paste("FOREX DATA/7-RULES/",y,"/","ALL",".csv",sep=""))
  
  dataf=as.data.frame(rbind(cbind("/","/",0)))
  cluster<- read_csv(paste("FOREX DATA/6-TRAIN/",y,"/","EURNZD",".csv",sep=""))
  df<-cluster[,16]
  df<-as.data.frame(df)
  dataf<-prepareData(df,8,dataf)
  dataf<-extendData(dataf)
  write_csv(dataf,path=paste("FOREX DATA/7-RULES/",y,"/","EURNZD",".csv",sep=""))
  
  dataf=as.data.frame(rbind(cbind("/","/",0)))
  cluster<- read_csv(paste("FOREX DATA/6-TRAIN/",y,"/","AUDCAD",".csv",sep=""))
  df<-cluster[,16]
  df<-as.data.frame(df)
  dataf<-prepareData(df,8,dataf)
  dataf<-extendData(dataf)
  write_csv(dataf,path=paste("FOREX DATA/7-RULES/",y,"/","AUDCAD",".csv",sep=""))
}


dataf

dataf[dataf$total>300,]

cluster<- read_csv(paste("FOREX DATA/4-PARTITIONED/CLUSTERS/","AUDCAD","_ForceCluster_5_Pearson.csv",sep=""))
df<-cluster[,16]
df<-as.data.frame(df)
dataf<-prepareData(df,6,dataf)

dataf[dataf$total>10,]

length(pairs)

extendData(dataf)

df<-force_cluster[,16]
df<-as.data.frame(df)
test<-prepareData(df,3,test)
dataf<-extendData(dataf)

test[test[,1]=="32",]

dataf[dataf[,5]>0.6,]

dataf
  
write.csv(dataf,file="forcecpearson5RulesAUDCAD_length6.csv")

write.csv2

tx<-prepareData(df,3)

extendData(tx)

tx<-dataf[dataf$total>10,]
tx[tx$confidence>0.5,]




prepareData<-function(df, sequenceLength,dataf=as.data.frame(rbind(cbind("/","/",0)))){
  colnames(dataf)[1]<-"antecedent"
  colnames(dataf)[2]<-"consequent"
  colnames(dataf)[3]<-"ocurrence"
  ocurrence<-1
  
  dataf[,1]<-as.character(dataf[,1])
  dataf[,2]<-as.character(dataf[,2])
  dataf[,3]<-as.numeric(dataf[,3])
  
  for(x in 2:nrow(df)){
    consequent<-as.character(df[x,1])
    antecedent<-""
    for(y in 1:sequenceLength){
      if(x-y>=1){
        antecedent<-paste(df[x-y,1],antecedent,sep="")
        if(sum(c(dataf[,1]==antecedent) + c(dataf[,2]==consequent) - 1==1)>=1){
          dataf[c(dataf[,1]==antecedent) + c(dataf[,2]==consequent) - 1==1,][,3]<-dataf[c(dataf[,1]==antecedent) + c(dataf[,2]==consequent) - 1==1,][,3]+1
        }else{
          dataf<-rbind(dataf,cbind(antecedent,consequent,ocurrence))
          dataf[,3]<-as.numeric(dataf[,3])
        }
      }
    }
  }
  if(dataf[1,1]=="/"){
    dataf<-dataf[2:nrow(dataf),]
  }
  return(dataf)
}

extendData<-function(dataf){
  for (x in 1:length(dataf[,1])) {
    dataf[x,4]<-sum(dataf[c(dataf[,1]==dataf[x,1]),][,3])
  }
  colnames(dataf)[4]<-"total"
  dataf[,5]<-dataf[,3]/dataf[,4]
  colnames(dataf)[5]<-"confidence"
  dataf[,6]<-stringi::stri_length(dataf[,1]) 
  colnames(dataf)[6]<-"size"
  x<--dataf[,5]
  y<--dataf[,3]
  z<--dataf[,4]
  dataf<-dataf[order(x,y,z),]
  return(dataf)
}
