#Para AUDCAD, EURNZD, USDJPY
#Usando Propio/ALL, propio/ALL, ALL.

#All Rules. Rules with over 60% confidence only. Rules with over 100 total only.

# Validar en test y medir error para las reglas obtenidas

#20 de test se solapa con train


rules<- read_csv(paste("FOREX DATA/7-RULES/Supervised3_05/","AUDCAD",".csv",sep=""))
test<-read_csv(paste("FOREX DATA/8-TEST/Supervised3_05/","USDJPY",".csv",sep=""))
price<- read_csv(paste("FOREX DATA/3-PROCESSED/HOURLY/","USDJPY",".csv",sep=""))

test<-test[13:nrow(test),] #length 8

cm<-validarReglas(test,price,rules,"AUDCAD","SelfAllRules",40)
write_csv(as.data.frame(cm),path=paste("FOREX DATA/9-RESULTS/Supervised3_05/","AUDCAD","SelfAllRulesCM.csv",sep=""))

rules<-rules[rules$confidence>0.6,]
cm<-validarReglas(test,price,rules,"AUDCAD","SelfConfidenceRules",40)
write_csv(as.data.frame(cm),path=paste("FOREX DATA/9-RESULTS/Supervised3_05/","AUDCAD","SelfConfidenceRulesCM.csv",sep=""))

rules<-rules[rules$total>5,]
cm<-validarReglas(test,price,rules,"AUDCAD","SelfConfidenceHighTotalRules",40)
write_csv(as.data.frame(cm),path=paste("FOREX DATA/9-RESULTS/Supervised3_05/","AUDCAD","SelfConfidenceHighTotalRulesCM.csv",sep=""))

rules<- read_csv(paste("FOREX DATA/7-RULES/Supervised3_05/","ALL",".csv",sep=""))

cm<-validarReglas(test,price,rules,"USDJPY","ALLAllRules",40)
write_csv(as.data.frame(cm),path=paste("FOREX DATA/9-RESULTS/Supervised3_05/","USDJPY","ALLAllRulesCM.csv",sep=""))

rules<-rules[rules$confidence>0.6,]
cm<-validarReglas(test,price,rules,"USDJPY","ALLConfidenceRules",40)
write_csv(as.data.frame(cm),path=paste("FOREX DATA/9-RESULTS/Supervised3_05/","USDJPY","ALLConfidenceRulesCM.csv",sep=""))

rules<-rules[rules$total>10,]
cm<-validarReglas(test,price,rules,"USDJPY","ALLConfidenceHighTotalRules",40)
write_csv(as.data.frame(cm),path=paste("FOREX DATA/9-RESULTS/Supervised3_05/","USDJPY","ALLConfidenceHighTotalRulesCM.csv",sep=""))




#"Self" "ALL"
#"AllRules" "ConfidenceRules" "ConfidenceHighTotalRules"

validarReglas<-function(segmentos,price,reglas,pair, name, limit){
  cm<-matrix(0,nrow=3,ncol=3)
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
    
    if(numpred!=2){
    units<-fundsnl/as.numeric(price[as.numeric(segmentos[x,1]),2])
    
    diff<-as.numeric(units*abs(as.numeric(price[as.numeric(segmentos[x,2]),2] - price[as.numeric(segmentos[x,1]),2])))
    val<-price[as.numeric(segmentos[x,2]),2] - price[as.numeric(segmentos[x,1]),2]
    diff<-ifelse((numpred==3 && val>0) || (numpred==1 && val<0),diff*-1,diff)
    fundsnl<-fundsnl+diff
    
    units<-fundsll/as.numeric(price[as.numeric(segmentos[x,1]),2])
    
    diff<-as.numeric(units*abs(as.numeric(price[as.numeric(segmentos[x,2]),2] - price[as.numeric(segmentos[x,1]),2])))
    val<-price[as.numeric(segmentos[x,2]),2] - price[as.numeric(segmentos[x,1]),2]
    diff<-ifelse((numpred==3 && val>0) || (numpred==1 && val<0),diff*-1,diff)
    fundsll<-fundsll+ifelse(diff<0,max(-fundsll*limit/10000,diff),diff) # limit losses
    
    units<-fundsbl/as.numeric(price[as.numeric(segmentos[x,1]),2])
    
    diff<-as.numeric(units*abs(as.numeric(price[as.numeric(segmentos[x,2]),2] - price[as.numeric(segmentos[x,1]),2])))
    val<-price[as.numeric(segmentos[x,2]),2] - price[as.numeric(segmentos[x,1]),2]
    diff<-ifelse((numpred==3 && val>0) || (numpred==1 && val<0),diff*-1,diff)
    fundsbl<-fundsbl+ifelse(diff<0,max(-fundsbl*limit/10000,diff),min(fundsbl*limit/10000,diff)) # limit both
    
    
    
    units<-10000/as.numeric(price[as.numeric(segmentos[x,1]),2])
    
    diff<-as.numeric(units*abs(as.numeric(price[as.numeric(segmentos[x,2]),2] - price[as.numeric(segmentos[x,1]),2])))
    val<-price[as.numeric(segmentos[x,2]),2] - price[as.numeric(segmentos[x,1]),2]
    diff<-ifelse((numpred==3 && val>0) || (numpred==1 && val<0),diff*-1,diff)

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
  
  write_csv(dfFunds,path=paste("FOREX DATA/9-RESULTS/Supervised3_05/",pair,name,"funds.csv",sep=""))
  return(cm)
}


predict<-function(sequence,rules){
  seq<-sequence
  pred<-""
  while(is.na(rules[rules[,1]==seq,][1,2])){
    seq<-stringr::str_sub(seq,2,50)
    if(seq==""){
      break
    }
  }
  pred<-NA
  if(seq!=""){
    pred<-rules[rules[,1]==seq,][1,]
  }
  return(pred)
}

path<-paste("FOREX DATA/9-RESULTS/","Price5","/","AUDCAD","self","AllRulesCM",".csv",sep="")
cm<-read_csv(path)



medidaError(as.matrix(cm))

medidaError<-function(cm){
  sum(diag(cm))/sum(cm)
}
