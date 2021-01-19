rules<- read_csv(paste("FOREX DATA/7-RULES/Price5/","EURNZD",".csv",sep=""))
test<-read_csv(paste("FOREX DATA/8-TEST/Price5/","USDJPY",".csv",sep=""))
price<- read_csv(paste("FOREX DATA/3-PROCESSED/HOURLY/","USDJPY",".csv",sep=""))

test<-test[15:nrow(test),] #length 6

cm<-validarReglas(test,price,rules,"EURNZD","SelfAllRules",40)
write_csv(as.data.frame(cm),path=paste("FOREX DATA/9-RESULTS/Price5/","EURNZD","SelfAllRulesCM.csv",sep=""))

rules<-rules[rules$confidence>0.6,]
cm<-validarReglas(test,price,rules,"EURNZD","SelfConfidenceRules",40)
write_csv(as.data.frame(cm),path=paste("FOREX DATA/9-RESULTS/Price5/","EURNZD","SelfConfidenceRulesCM.csv",sep=""))

rules<-rules[rules$total>5,]
cm<-validarReglas(test,price,rules,"EURNZD","SelfConfidenceHighTotalRules",40)
write_csv(as.data.frame(cm),path=paste("FOREX DATA/9-RESULTS/Price5/","EURNZD","SelfConfidenceHighTotalRulesCM.csv",sep=""))

rules<- read_csv(paste("FOREX DATA/7-RULES/Price5/","ALL",".csv",sep=""))

cm<-validarReglas(test,price,rules,"USDJPY","ALLAllRules",40)
write_csv(as.data.frame(cm),path=paste("FOREX DATA/9-RESULTS/Price5/","USDJPY","ALLAllRulesCM.csv",sep=""))

rules<-rules[rules$confidence>0.6,]
cm<-validarReglas(test,price,rules,"USDJPY","ALLConfidenceRules",40)
write_csv(as.data.frame(cm),path=paste("FOREX DATA/9-RESULTS/Price5/","USDJPY","ALLConfidenceRulesCM.csv",sep=""))

rules<-rules[rules$total>10,]
cm<-validarReglas(test,price,rules,"USDJPY","ALLConfidenceHighTotalRules",40)
write_csv(as.data.frame(cm),path=paste("FOREX DATA/9-RESULTS/Price5/","USDJPY","ALLConfidenceHighTotalRulesCM.csv",sep=""))


 #length 6

#"Self" "ALL"
#"AllRules" "ConfidenceRules" "ConfidenceHighTotalRules"

validarReglas<-function(segmentos,price,reglas,pair, name, limit){
  cm<-matrix(0,nrow=nrow(unique(reglas[,2])),ncol=nrow(unique(reglas[,2])))
  seqLen<-max(reglas[,6])
  sequence<-""
  fundsnl<-10000
  fixednl<-10000
  
  fundsll<-10000
  fixedll<-10000
  
  fundsbl<-10000
  fixedbl<-10000
  
  for(x in 1:seqLen){
    sequence<-paste(sequence,segmentos[x,16],sep="")
  }
  vectorfundsnl<-c(10000)
  vectorfixednl<-c(10000)
  
  vectorfundsll<-c(10000)
  vectorfixedll<-c(10000)
  
  vectorfundsbl<-c(10000)
  vectorfixedbl<-c(10000)
  
  for(x in (seqLen+1):nrow(segmentos)){ #nrow(segmentos)
    pred<-predict(sequence,reglas)
    segGroup<-as.numeric(segmentos[x,16])
    if(!is.na(pred)){
      numpred<-as.numeric(pred[1,2])
      cm[numpred,segGroup]<-cm[numpred,segGroup]+1
      
      if(numpred!=3){
        units<-fundsnl/as.numeric(price[as.numeric(segmentos[x,1]),2])
        
        diff<-as.numeric(units*abs(as.numeric(price[as.numeric(segmentos[x,2]),2] - price[as.numeric(segmentos[x,1]),2])))
        val<-price[as.numeric(segmentos[x,2]),2] - price[as.numeric(segmentos[x,1]),2]
        diff<-ifelse(((numpred==5 || numpred==4) && val>0) || ((numpred==1 || numpred==2) && val<0),diff*-1,diff)
        fundsnl<-fundsnl+diff
        
        units<-fundsll/as.numeric(price[as.numeric(segmentos[x,1]),2])
        
        diff<-as.numeric(units*abs(as.numeric(price[as.numeric(segmentos[x,2]),2] - price[as.numeric(segmentos[x,1]),2])))
        val<-price[as.numeric(segmentos[x,2]),2] - price[as.numeric(segmentos[x,1]),2]
        diff<-ifelse(((numpred==5 || numpred==4) && val>0) || ((numpred==1 || numpred==2) && val<0),diff*-1,diff)
        fundsll<-fundsll+ifelse(diff<0,max(-fundsll*limit/10000,diff),diff) # limit losses
        
        units<-fundsbl/as.numeric(price[as.numeric(segmentos[x,1]),2])
        
        diff<-as.numeric(units*abs(as.numeric(price[as.numeric(segmentos[x,2]),2] - price[as.numeric(segmentos[x,1]),2])))
        val<-price[as.numeric(segmentos[x,2]),2] - price[as.numeric(segmentos[x,1]),2]
        diff<-ifelse(((numpred==5 || numpred==4) && val>0) || ((numpred==1 || numpred==2) && val<0),diff*-1,diff)
        fundsbl<-fundsbl+ifelse(diff<0,max(-fundsbl*limit/10000,diff),min(fundsbl*limit/10000,diff)) # limit both
        
        
        
        units<-10000/as.numeric(price[as.numeric(segmentos[x,1]),2])
        
        diff<-as.numeric(units*abs(as.numeric(price[as.numeric(segmentos[x,2]),2] - price[as.numeric(segmentos[x,1]),2])))
        val<-price[as.numeric(segmentos[x,2]),2] - price[as.numeric(segmentos[x,1]),2]
        diff<-ifelse(((numpred==5 || numpred==4) && val>0) || ((numpred==1 || numpred==2) && val<0),diff*-1,diff)
        
        fixednl<-fixednl+diff
        fixedll<-fixedll+ifelse(diff<0,max(-limit,diff),diff) # limit losses
        fixedbl<-fixedbl+ifelse(diff<0,max(-limit,diff),min(limit,diff)) #limit both
        
        vectorfundsnl<-c(vectorfundsnl,fundsnl)
        vectorfixednl<-c(vectorfixednl,fixednl)
        
        vectorfundsll<-c(vectorfundsll,fundsll)
        vectorfixedll<-c(vectorfixedll,fixedll)
        
        vectorfundsbl<-c(vectorfundsbl,fundsbl)
        vectorfixedbl<-c(vectorfixedbl,fixedbl)
        
      }
      
    }
    sequence<-stringr::str_sub(sequence,2,seqLen)
    sequence<-paste(sequence,segGroup,sep="")
  }
  vectorfundsnl<-as.data.frame(vectorfundsnl)
  vectorfixednl<-as.data.frame(vectorfixednl)
  vectorfundsll<-as.data.frame(vectorfundsll)
  vectorfixedll<-as.data.frame(vectorfixedll)
  vectorfundsbl<-as.data.frame(vectorfundsbl)
  vectorfixedbl<-as.data.frame(vectorfixedbl)
  dfFunds<-cbind(vectorfundsnl,vectorfixednl,vectorfundsll,vectorfixedll,vectorfundsbl,vectorfixedbl)
  colnames(dfFunds)[1]<-"No limit funds %"
  colnames(dfFunds)[2]<-"No limit funds fixed"
  colnames(dfFunds)[3]<-"Loss limit funds %"
  colnames(dfFunds)[4]<-"Loss limit funds fixed"
  colnames(dfFunds)[5]<-"Both limit funds %"
  colnames(dfFunds)[6]<-"Both limit funds fixed"
  
  write_csv(dfFunds,path=paste("FOREX DATA/9-RESULTS/Price5/",pair,name,"funds.csv",sep=""))
  return(cm)
}
