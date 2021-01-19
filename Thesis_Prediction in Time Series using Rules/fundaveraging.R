SinLimite<-0
SinLimiteFijo<-0
LimitePerdida<-0
LimitePerdidaFijo<-0
LimiteAmbos<-0
LimiteAmbosFijo<-0
df1<-data.frame(SinLimite,SinLimiteFijo,LimitePerdida,LimitePerdidaFijo,LimiteAmbos,LimiteAmbosFijo)
df2<-data.frame(SinLimite,SinLimiteFijo,LimitePerdida,LimitePerdidaFijo,LimiteAmbos,LimiteAmbosFijo)
df3<-data.frame(SinLimite,SinLimiteFijo,LimitePerdida,LimitePerdidaFijo,LimiteAmbos,LimiteAmbosFijo)

loop<-c("Price5","Force5","Price5Except3","Force5Except3","Slope3","SlopeVar3","Supervised3_05","Supervised3","Supervised3_15","Supervised3_20","SupervisedBin")
for(x in 1:length(loop)){
  count<-1
  
  avg<-data.frame(SinLimite,SinLimiteFijo,LimitePerdida,LimitePerdidaFijo,LimiteAmbos,LimiteAmbosFijo)
  for(y in c("AUDCAD","EURNZD")){
    path<-paste("FOREX DATA/9-RESULTS/",loop[x],"/",y,"Self","Allrulesfunds",".csv",sep="")
    cm<-read_csv(path)
    avg<-avg+cm[nrow(cm),]
  }
  avg<-avg/2
  df1[x,]<-avg
  
  avg<-data.frame(SinLimite,SinLimiteFijo,LimitePerdida,LimitePerdidaFijo,LimiteAmbos,LimiteAmbosFijo)
  for(y in c("AUDCAD","EURNZD")){
    path<-paste("FOREX DATA/9-RESULTS/",loop[x],"/",y,"Self","ConfidenceRulesfunds",".csv",sep="")
    cm<-read_csv(path)
    avg<-avg+cm[nrow(cm),]
  }
  avg<-avg/2
  df2[x,]<-avg
  
  avg<-data.frame(SinLimite,SinLimiteFijo,LimitePerdida,LimitePerdidaFijo,LimiteAmbos,LimiteAmbosFijo)
  for(y in c("AUDCAD","EURNZD")){
    path<-paste("FOREX DATA/9-RESULTS/",loop[x],"/",y,"Self","ConfidenceHighTotalRulesfunds",".csv",sep="")
    cm<-read_csv(path)
    avg<-avg+cm[nrow(cm),]
  }
  avg<-avg/2
  df3[x,]<-avg
  count<-count+1


}
loopn<-c("Precio5","Fuerza5","Precio5Ruido3","Fuerza5Ruido3","Pendiente3","PendienteVar3","Supervisado3_05","Supervisado3_10","Supervisado3_15","Supervisado3_20","SupervisadoBin")



df1[,7]<-loopn
df2[,7]<-loopn
df3[,7]<-loopn

df1

write_csv(as.data.frame(df1),path=paste("FOREX DATA/9-RESULTS/SelfAllRulesFunds.csv",sep=""))
write_csv(as.data.frame(df2),path=paste("FOREX DATA/9-RESULTS/SelfConfRulesFunds.csv",sep=""))
write_csv(as.data.frame(df3),path=paste("FOREX DATA/9-RESULTS/SelfTotRulesFunds.csv",sep=""))
