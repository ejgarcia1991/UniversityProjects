#Cargo las librerías que voy a utilizar en el código
library(readr)
library(fpc)
library(cluster)
library(factoextra)

#para documento
segment<- read_csv(paste("FOREX DATA/4-PARTITIONED/SEGMENTS/","AUDCAD","_Segments.csv",sep=""))
segment<- as.data.frame(segment)
segment1<-segment
segment1<-scale(segment1[,3:12])[,]

fviz_nbclust(segment1[,6:10],kmeans,method="silhouette")

fviz_nbclust(segment1[,6:10],kmeans,method="silhouette",diss=dist1)

dist1<-dist(segment1[,6:10])
set.seed(18)
kmeans.result=kmeans(dist2,3,iter.max = 15,nstart=5)
grupo=kmeans.result$cluster
shi= silhouette(grupo,dist2)
plot(shi,col=1:3)

p<-c(0.41,0.45,0.47,0.575,0.58,0.57,0.56,0.55,0.55,0.54)
plot(p,type="l",xlab="Number of clusters",ylab="silhouette")

dist1<-get_dist(segment1[,1:5],method="spearman")

dist2

segment1[,c(1,5,10)]

dist2<-get_dist(segment1[1:5,c(1:10)],method="spearman")
dist2<-get_dist(segment1[,c(1,5,10)],method="pearson")
fviz_dist(dist2)

for(x in pairs){
segment<- read_csv(paste("FOREX DATA/4-SEGMENTS/",x,"_.csv",sep=""))
segment<- as.data.frame(segment)
grupo<-ifelse(segment$slope>0,1,2)
write_csv(cbind(segment,grupo),path=paste("FOREX DATA/5-CLUSTERS/SupervisedBinPrecio/",x,"_Ord.csv",sep=""))
}

for(x in pairs){
  segment<- read_csv(paste("FOREX DATA/4-SEGMENTS/",x,"_.csv",sep=""))
  segment<- as.data.frame(segment)
  segment1<-segment
  segment1<-scale(segment1[,3:12])[,]
  
  dist1<-get_dist(segment1[,1:5],method="pearson")
  set.seed(18)
  kmeans.result=kmeans(dist1,5,iter.max = 20,nstart=10)
  grupo=kmeans.result$cluster
  write_csv(cbind(segment,grupo),path=paste("FOREX DATA/5-CLUSTERS/Price5/",x,".csv",sep=""))
  
  dist1<-get_dist(segment1[,6:10],method="pearson")
  set.seed(18)
  kmeans.result=kmeans(dist1,5,iter.max = 20,nstart=10)
  grupo=kmeans.result$cluster
  write_csv(cbind(segment,grupo),path=paste("FOREX DATA/5-CLUSTERS/Force5/",x,".csv",sep=""))
  
  dist1<-get_dist(segment1[,c(5,10)],method="pearson")
  set.seed(18)
  kmeans.result=kmeans(dist1,3,iter.max = 20,nstart=10)
  grupo=kmeans.result$cluster
  write_csv(cbind(segment,grupo),path=paste("FOREX DATA/5-CLUSTERS/Slope3/",x,".csv",sep=""))
  
  dist1<-get_dist(segment1[,c(1,5,10)],method="pearson")
  set.seed(18)
  kmeans.result=kmeans(dist1,3,iter.max = 20,nstart=10)
  grupo=kmeans.result$cluster
  write_csv(cbind(segment,grupo),path=paste("FOREX DATA/5-CLUSTERS/SlopeVar3/",x,".csv",sep=""))
  
  
  grupo<-ifelse(segment$Fslope>0.1,1,ifelse(-0.1>segment$Fslope,3,2))
  write_csv(cbind(segment,grupo),path=paste("FOREX DATA/5-CLUSTERS/Supervised3/",x,"_Ord.csv",sep=""))
  
  grupo<-ifelse(segment$Fslope>0.15,1,ifelse(-0.15>segment$Fslope,3,2))
  write_csv(cbind(segment,grupo),path=paste("FOREX DATA/5-CLUSTERS/Supervised3_15/",x,"_Ord.csv",sep=""))
  
  grupo<-ifelse(segment$Fslope>0.2,1,ifelse(-0.2>segment$Fslope,3,2))
  write_csv(cbind(segment,grupo),path=paste("FOREX DATA/5-CLUSTERS/Supervised3_20/",x,"_Ord.csv",sep=""))
  
  segment<- read_csv(paste("FOREX DATA/4-SEGMENTS/",x,"_.csv",sep=""))
  segment<- as.data.frame(segment)
  grupo<-ifelse(segment$Fslope>0.05,1,ifelse(-0.05>segment$Fslope,3,2))
  write_csv(cbind(segment,grupo),path=paste("FOREX DATA/5-CLUSTERS/Supervised3_05/",x,"_Ord.csv",sep=""))
  
  segment<- read_csv(paste("FOREX DATA/4-SEGMENTS/",x,"_.csv",sep=""))
  segment<- as.data.frame(segment)
  grupo<-ifelse(segment$slope>0,1,2)
  write_csv(cbind(segment,grupo),path=paste("FOREX DATA/5-CLUSTERS/SupervisedBin/",x,"_Ord.csv",sep=""))
  
} #generate Clusters

for(x in pairs){
  cluster<- read_csv(paste("FOREX DATA/5-CLUSTERS/SlopeVar3/",x,".csv",sep=""))
  pos<-c(mean(cluster$slope[cluster$grupo==1]),mean(cluster$slope[cluster$grupo==2]),mean(cluster$slope[cluster$grupo==3]))
  for(z in 2:3){
    for(y in z:(1+1)){
      if(pos[y]>pos[y-1]){
        cluster<-swap(cluster,y,y-1)
        temp<-pos[y]
        pos[y]<-pos[y-1]
        pos[y-1]<-temp
      }
    }
  }
  write_csv(cluster,path=paste("FOREX DATA/5-CLUSTERS/SlopeVar3/",x,"_Ord.csv",sep=""))
  
  cluster<- read_csv(paste("FOREX DATA/5-CLUSTERS/Slope3/",x,".csv",sep=""))
  pos<-c(mean(cluster$slope[cluster$grupo==1]),mean(cluster$slope[cluster$grupo==2]),mean(cluster$slope[cluster$grupo==3]))
  for(z in 2:3){
    for(y in z:(1+1)){
      if(pos[y]>pos[y-1]){
        cluster<-swap(cluster,y,y-1)
        temp<-pos[y]
        pos[y]<-pos[y-1]
        pos[y-1]<-temp
      }
    }
  }
  write_csv(cluster,path=paste("FOREX DATA/5-CLUSTERS/Slope3/",x,"_Ord.csv",sep=""))
  
  cluster<- read_csv(paste("FOREX DATA/5-CLUSTERS/Force5/",x,".csv",sep=""))
  pos<-c(mean(cluster$slope[cluster$grupo==1]),mean(cluster$slope[cluster$grupo==2]),mean(cluster$slope[cluster$grupo==3]),mean(cluster$slope[cluster$grupo==4]),mean(cluster$slope[cluster$grupo==5]))
  for(z in 2:5){
    for(y in z:(1+1)){
      if(pos[y]>pos[y-1]){
        cluster<-swap(cluster,y,y-1)
        temp<-pos[y]
        pos[y]<-pos[y-1]
        pos[y-1]<-temp
      }
    }
  }
  write_csv(cluster,path=paste("FOREX DATA/5-CLUSTERS/Force5/",x,"_Ord.csv",sep=""))
  
  cluster<- read_csv(paste("FOREX DATA/5-CLUSTERS/Price5/",x,".csv",sep=""))
  pos<-c(mean(cluster$slope[cluster$grupo==1]),mean(cluster$slope[cluster$grupo==2]),mean(cluster$slope[cluster$grupo==3]),mean(cluster$slope[cluster$grupo==4]),mean(cluster$slope[cluster$grupo==5]))
  for(z in 2:5){
    for(y in z:(1+1)){
      if(pos[y]>pos[y-1]){
        cluster<-swap(cluster,y,y-1)
        temp<-pos[y]
        pos[y]<-pos[y-1]
        pos[y-1]<-temp
      }
    }
  }
  write_csv(cluster,path=paste("FOREX DATA/5-CLUSTERS/Price5/",x,"_Ord.csv",sep=""))
} #order Clusters


swap<-function(clust,pos1,pos2){
  cluster<-clust
  cluster[cluster$grupo==pos1,16]<-999
  cluster[cluster$grupo==pos2,16]<-pos1
  cluster[cluster$grupo==999,16]<-pos2
  return(cluster)
}

#Visualización
PrecioSegmentadoCat<-function(spot,clusters,timetag){
  st<-clusters[,1]
  en<-clusters[,2]
  pri<-cbind(timetag[st[1]],spot[st[1]],clusters[1,16])
  for(x in (st[1]+1):length(spot)){
    for(y in 1:length(en)){
      if(x>st[y] && x<=en[y]){
        pri<-rbind(pri,cbind(timetag[x],spot[x],clusters[y,16]))
      }
    }
  }
  return(pri)
}

price_cluster<-read.csv(paste("FOREX DATA/4-PARTITIONED/CLUSTERS/","AUDCAD","_PriceCluster_5_Pearson.csv",sep=""))
force_cluster<-read.csv(paste("FOREX DATA/4-PARTITIONED/CLUSTERS/","AUDCAD","_ForceCluster_5_Pearson.csv",sep=""))


price_cluster<-read.csv(paste("FOREX DATA/4-PARTITIONED/CLUSTERS/","AUDCAD","_PriceCluster_5.csv",sep=""))
force_cluster<-read.csv(paste("FOREX DATA/4-PARTITIONED/CLUSTERS/","AUDCAD","_ForceCluster.csv",sep=""))
price_cluster<-read.csv(paste("FOREX DATA/4-PARTITIONED/CLUSTERS/","AUDCAD","_PriceCluster.csv",sep=""))

ftest<-PrecioSegmentadoCat(AUDCAD$Open,cbind(segment,grupo),cleanTimeFrame(AUDCAD$Time))

ftest<-PrecioSegmentadoCat(AUDCAD$Open,force_cluster,cleanTimeFrame(AUDCAD$Time))

ftest<-as.data.frame(ftest)
ftest[,1]<-as.POSIXct(ftest[,1],origin="1970-01-01",tz="UTC")
ftest[,3]<-as.factor(ftest[,3])

#Visual en forma de serie
library(ggplot2)
small<-tail(ftest,1000)
small$V1<-1:1000


segments<-list()
change<-1
for(x in 2:1000){
  if(small[x,3]!=small[x-1,3]){
    segments[[length(segments)+1]]<-small[change:x-1,]
    change<-x
  }
}

ggplot()+
  geom_line(aes(x=V1,y=V2,color=segments[[1]][2,3]),segments[[1]])+
  geom_line(aes(x=V1,y=V2,color=segments[[2]][2,3]),segments[[2]])+
  geom_line(aes(x=V1,y=V2,color=segments[[3]][2,3]),segments[[3]])+
  geom_line(aes(x=V1,y=V2,color=segments[[4]][2,3]),segments[[4]])+
  geom_line(aes(x=V1,y=V2,color=segments[[5]][2,3]),segments[[5]])+
  geom_line(aes(x=V1,y=V2,color=segments[[6]][2,3]),segments[[6]])+
  geom_line(aes(x=V1,y=V2,color=segments[[7]][2,3]),segments[[7]])+
  geom_line(aes(x=V1,y=V2,color=segments[[8]][2,3]),segments[[8]])+
  geom_line(aes(x=V1,y=V2,color=segments[[9]][2,3]),segments[[9]])+
  geom_line(aes(x=V1,y=V2,color=segments[[10]][2,3]),segments[[10]])+
  geom_line(aes(x=V1,y=V2,color=segments[[11]][2,3]),segments[[11]])+
  geom_line(aes(x=V1,y=V2,color=segments[[12]][2,3]),segments[[12]])+
  geom_line(aes(x=V1,y=V2,color=segments[[13]][2,3]),segments[[13]])+
  geom_line(aes(x=V1,y=V2,color=segments[[14]][2,3]),segments[[14]])+
  geom_line(aes(x=V1,y=V2,color=segments[[15]][2,3]),segments[[15]])+
  geom_line(aes(x=V1,y=V2,color=segments[[16]][2,3]),segments[[16]])+
  geom_line(aes(x=V1,y=V2,color=segments[[17]][2,3]),segments[[17]])+
  geom_line(aes(x=V1,y=V2,color=segments[[18]][2,3]),segments[[18]])+
  geom_line(aes(x=V1,y=V2,color=segments[[19]][2,3]),segments[[19]])+
  geom_line(aes(x=V1,y=V2,color=segments[[20]][2,3]),segments[[20]])+
  geom_line(aes(x=V1,y=V2,color=segments[[21]][2,3]),segments[[21]])+
  geom_line(aes(x=V1,y=V2,color=segments[[22]][2,3]),segments[[22]])+
  geom_line(aes(x=V1,y=V2,color=segments[[23]][2,3]),segments[[23]])+
  geom_line(aes(x=V1,y=V2,color=segments[[24]][2,3]),segments[[24]])+
  geom_line(aes(x=V1,y=V2,color=segments[[25]][2,3]),segments[[25]])+
  geom_line(aes(x=V1,y=V2,color=segments[[26]][2,3]),segments[[26]])+
  geom_line(aes(x=V1,y=V2,color=segments[[27]][2,3]),segments[[27]])+
  geom_line(aes(x=V1,y=V2,color=segments[[28]][2,3]),segments[[28]])+
  geom_line(aes(x=V1,y=V2,color=segments[[29]][2,3]),segments[[29]])+
  geom_line(aes(x=V1,y=V2,color=segments[[30]][2,3]),segments[[30]])+
  geom_line(aes(x=V1,y=V2,color=segments[[31]][2,3]),segments[[31]])+
  geom_line(aes(x=V1,y=V2,color=segments[[32]][2,3]),segments[[32]])+
  geom_line(aes(x=V1,y=V2,color=segments[[33]][2,3]),segments[[33]])+
  geom_line(aes(x=V1,y=V2,color=segments[[34]][2,3]),segments[[34]])+
  geom_line(aes(x=V1,y=V2,color=segments[[35]][2,3]),segments[[35]])+
  geom_line(aes(x=V1,y=V2,color=segments[[36]][2,3]),segments[[36]])+
  geom_line(aes(x=V1,y=V2,color=segments[[37]][2,3]),segments[[37]])+
  geom_line(aes(x=V1,y=V2,color=segments[[38]][2,3]),segments[[38]])+
  geom_line(aes(x=V1,y=V2,color=segments[[39]][2,3]),segments[[39]])+
  geom_line(aes(x=V1,y=V2,color=segments[[40]][2,3]),segments[[40]])+
  geom_line(aes(x=V1,y=V2,color=segments[[41]][2,3]),segments[[41]])+
  geom_line(aes(x=V1,y=V2,color=segments[[42]][2,3]),segments[[42]])+
  geom_line(aes(x=V1,y=V2,color=segments[[43]][2,3]),segments[[43]])+
  geom_line(aes(x=V1,y=V2,color=segments[[44]][2,3]),segments[[44]])+
  geom_line(aes(x=V1,y=V2,color=segments[[45]][2,3]),segments[[45]])+
#  geom_line(aes(x=V1,y=V2,color=segments[[46]][2,3]),segments[[46]])+
#  geom_line(aes(x=V1,y=V2,color=segments[[47]][2,3]),segments[[47]])+
#  geom_line(aes(x=V1,y=V2,color=segments[[48]][2,3]),segments[[48]])+
#  geom_line(aes(x=V1,y=V2,color=segments[[49]][2,3]),segments[[49]])+
#  geom_line(aes(x=V1,y=V2,color=segments[[50]][2,3]),segments[[50]])+
#  geom_line(aes(x=V1,y=V2,color=segments[[51]][2,3]),segments[[51]])+
  xlab("Time")+
  ylab("Price")+
  scale_color_manual(name="cluster",values = c("#0DDADD","#000000","#EF0321","#FFAA00","green"))+
  theme_minimal()

ggplot()+
  geom_line(aes(x=V1,y=V2,color=segments[[1]][2,3]),linetype=as.integer(segments[[1]][2,3]),segments[[1]])+
  geom_line(aes(x=V1,y=V2,color=segments[[2]][2,3]),linetype=as.integer(segments[[2]][2,3]),segments[[2]])+
  geom_line(aes(x=V1,y=V2,color=segments[[3]][2,3]),linetype=as.integer(segments[[3]][2,3]),segments[[3]])+
  geom_line(aes(x=V1,y=V2,color=segments[[4]][2,3]),linetype=as.integer(segments[[4]][2,3]),segments[[4]])+
  geom_line(aes(x=V1,y=V2,color=segments[[5]][2,3]),linetype=as.integer(segments[[5]][2,3]),segments[[5]])+
  geom_line(aes(x=V1,y=V2,color=segments[[6]][2,3]),linetype=as.integer(segments[[6]][2,3]),segments[[6]])+
  geom_line(aes(x=V1,y=V2,color=segments[[7]][2,3]),linetype=as.integer(segments[[7]][2,3]),segments[[7]])+
  geom_line(aes(x=V1,y=V2,color=segments[[8]][2,3]),linetype=as.integer(segments[[8]][2,3]),segments[[8]])+
  geom_line(aes(x=V1,y=V2,color=segments[[9]][2,3]),linetype=as.integer(segments[[9]][2,3]),segments[[9]])+
  geom_line(aes(x=V1,y=V2,color=segments[[10]][2,3]),linetype=as.integer(segments[[10]][2,3]),segments[[10]])+
  geom_line(aes(x=V1,y=V2,color=segments[[11]][2,3]),linetype=as.integer(segments[[11]][2,3]),segments[[11]])+
  geom_line(aes(x=V1,y=V2,color=segments[[12]][2,3]),linetype=as.integer(segments[[12]][2,3]),segments[[12]])+
  geom_line(aes(x=V1,y=V2,color=segments[[13]][2,3]),linetype=as.integer(segments[[13]][2,3]),segments[[13]])+
  geom_line(aes(x=V1,y=V2,color=segments[[14]][2,3]),linetype=as.integer(segments[[14]][2,3]),segments[[14]])+
  geom_line(aes(x=V1,y=V2,color=segments[[15]][2,3]),linetype=as.integer(segments[[15]][2,3]),segments[[15]])+
  geom_line(aes(x=V1,y=V2,color=segments[[16]][2,3]),linetype=as.integer(segments[[16]][2,3]),segments[[16]])+
  geom_line(aes(x=V1,y=V2,color=segments[[17]][2,3]),linetype=as.integer(segments[[17]][2,3]),segments[[17]])+
  geom_line(aes(x=V1,y=V2,color=segments[[18]][2,3]),linetype=as.integer(segments[[18]][2,3]),segments[[18]])+
  geom_line(aes(x=V1,y=V2,color=segments[[19]][2,3]),linetype=as.integer(segments[[19]][2,3]),segments[[19]])+
  geom_line(aes(x=V1,y=V2,color=segments[[20]][2,3]),linetype=as.integer(segments[[20]][2,3]),segments[[20]])+
  geom_line(aes(x=V1,y=V2,color=segments[[21]][2,3]),linetype=as.integer(segments[[21]][2,3]),segments[[21]])+
  geom_line(aes(x=V1,y=V2,color=segments[[22]][2,3]),linetype=as.integer(segments[[22]][2,3]),segments[[22]])+
  geom_line(aes(x=V1,y=V2,color=segments[[23]][2,3]),linetype=as.integer(segments[[23]][2,3]),segments[[23]])+
  geom_line(aes(x=V1,y=V2,color=segments[[24]][2,3]),linetype=as.integer(segments[[24]][2,3]),segments[[24]])+
  geom_line(aes(x=V1,y=V2,color=segments[[25]][2,3]),linetype=as.integer(segments[[25]][2,3]),segments[[25]])+
  geom_line(aes(x=V1,y=V2,color=segments[[26]][2,3]),linetype=as.integer(segments[[26]][2,3]),segments[[26]])+
  geom_line(aes(x=V1,y=V2,color=segments[[27]][2,3]),linetype=as.integer(segments[[27]][2,3]),segments[[27]])+
  geom_line(aes(x=V1,y=V2,color=segments[[28]][2,3]),linetype=as.integer(segments[[28]][2,3]),segments[[28]])+
  geom_line(aes(x=V1,y=V2,color=segments[[29]][2,3]),linetype=as.integer(segments[[29]][2,3]),segments[[29]])+
  geom_line(aes(x=V1,y=V2,color=segments[[30]][2,3]),linetype=as.integer(segments[[30]][2,3]),segments[[30]])+
  geom_line(aes(x=V1,y=V2,color=segments[[31]][2,3]),linetype=as.integer(segments[[31]][2,3]),segments[[31]])+
  geom_line(aes(x=V1,y=V2,color=segments[[32]][2,3]),linetype=as.integer(segments[[32]][2,3]),segments[[32]])+
  geom_line(aes(x=V1,y=V2,color=segments[[33]][2,3]),linetype=as.integer(segments[[33]][2,3]),segments[[33]])+
  geom_line(aes(x=V1,y=V2,color=segments[[34]][2,3]),linetype=as.integer(segments[[34]][2,3]),segments[[34]])+
  geom_line(aes(x=V1,y=V2,color=segments[[35]][2,3]),linetype=as.integer(segments[[35]][2,3]),segments[[35]])+
  geom_line(aes(x=V1,y=V2,color=segments[[36]][2,3]),linetype=as.integer(segments[[36]][2,3]),segments[[36]])+
  geom_line(aes(x=V1,y=V2,color=segments[[37]][2,3]),linetype=as.integer(segments[[37]][2,3]),segments[[37]])+
  geom_line(aes(x=V1,y=V2,color=segments[[38]][2,3]),linetype=as.integer(segments[[38]][2,3]),segments[[38]])+
  geom_line(aes(x=V1,y=V2,color=segments[[39]][2,3]),linetype=as.integer(segments[[39]][2,3]),segments[[39]])+
  geom_line(aes(x=V1,y=V2,color=segments[[40]][2,3]),linetype=as.integer(segments[[40]][2,3]),segments[[40]])+
  geom_line(aes(x=V1,y=V2,color=segments[[41]][2,3]),linetype=as.integer(segments[[41]][2,3]),segments[[41]])+
  geom_line(aes(x=V1,y=V2,color=segments[[42]][2,3]),linetype=as.integer(segments[[42]][2,3]),segments[[42]])+
  geom_line(aes(x=V1,y=V2,color=segments[[43]][2,3]),linetype=as.integer(segments[[43]][2,3]),segments[[43]])+
  geom_line(aes(x=V1,y=V2,color=segments[[44]][2,3]),linetype=as.integer(segments[[44]][2,3]),segments[[44]])+
  geom_line(aes(x=V1,y=V2,color=segments[[45]][2,3]),linetype=as.integer(segments[[45]][2,3]),segments[[45]])+
  geom_line(aes(x=V1,y=V2,color=segments[[46]][2,3]),linetype=as.integer(segments[[46]][2,3]),segments[[46]])+
  geom_line(aes(x=V1,y=V2,color=segments[[47]][2,3]),linetype=as.integer(segments[[47]][2,3]),segments[[47]])+
  geom_line(aes(x=V1,y=V2,color=segments[[48]][2,3]),linetype=as.integer(segments[[48]][2,3]),segments[[48]])+
  geom_line(aes(x=V1,y=V2,color=segments[[49]][2,3]),linetype=as.integer(segments[[49]][2,3]),segments[[49]])+
  geom_line(aes(x=V1,y=V2,color=segments[[50]][2,3]),linetype=as.integer(segments[[50]][2,3]),segments[[50]])+
  xlab("Time")+
  ylab("Price")+
  scale_color_manual(values = c("#0DDADD","#000000","#EF0321","#FFAA00","green"))+
  theme_minimal()


for(x in pairs){
  segment<- read_csv(paste("FOREX DATA/4-PARTITIONED/SEGMENTS/",x,"_Segments.csv",sep=""))
  segment<- as.data.frame(segment)
  segment1<-segment
  segment1<-scale(segment1[,3:12])[,]
  set.seed(18)
  kmeans.result=kmeans(segment1[,1:5],5,iter.max = 100,nstart=50)
  grupo=kmeans.result$cluster
  write_csv(cbind(segment,grupo),path=paste("FOREX DATA/4-PARTITIONED/CLUSTERS/",x,"_PriceCluster_5.csv",sep=""))
  set.seed(18)
  kmeans.result=kmeans(segment1[,1:5],2,iter.max = 100,nstart=50)
  grupo=kmeans.result$cluster
  write_csv(cbind(segment,grupo),path=paste("FOREX DATA/4-PARTITIONED/CLUSTERS/",x,"_PriceCluster.csv",sep=""))
  set.seed(18)
  kmeans.result=kmeans(segment1[,6:10],5,iter.max = 100,nstart=50)
  grupo=kmeans.result$cluster
  write_csv(cbind(segment,grupo),path=paste("FOREX DATA/4-PARTITIONED/CLUSTERS/",x,"_ForceCluster.csv",sep=""))
}

for(x in pairs){
  segment<- read_csv(paste("FOREX DATA/4-PARTITIONED/SEGMENTS/",x,"_Segments.csv",sep=""))
  segment<- as.data.frame(segment)
  segment1<-segment
  segment1<-scale(segment1[,3:12])[,]
  dist1<-get_dist(segment1[,1:5],method="pearson")
  set.seed(18)
  kmeans.result=kmeans(dist1,5,iter.max = 20,nstart=10)
  grupo=kmeans.result$cluster
  write_csv(cbind(segment,grupo),path=paste("FOREX DATA/4-PARTITIONED/CLUSTERS/",x,"_PriceCluster_5_Pearson.csv",sep=""))
  dist1<-get_dist(segment1[,6:10],method="pearson")
  set.seed(18)
  kmeans.result=kmeans(dist1,5,iter.max = 20,nstart=10)
  grupo=kmeans.result$cluster
  write_csv(cbind(segment,grupo),path=paste("FOREX DATA/4-PARTITIONED/CLUSTERS/",x,"_ForceCluster_5_Pearson.csv",sep=""))
}