cleanVectorOfImmobileValues<-function(vector){
  aliveIndexes<-rep(TRUE,length(vector))
  for(x in 2:length(vector)){
    if(is.na(vector[x-1])||is.na(vector[x])){
      aliveIndexes[x]<-FALSE
    }else{
    if(vector[x-1]==vector[x]){
      aliveIndexes[x]<-FALSE
    }
      }
  }
  return(vector[aliveIndexes])
}

calculateMeanOfTemporalSeries<-function(vector,meanLength){
  result<-rep(NA, length(vector))
  for (x in meanLength:length(vector)) {
    if(is.na(vector[x])||is.na(vector[x-1])){
      result[x]<-NA
    }else{
      extendedLength<-meanLength
      unfinished<-TRUE
      notFound<-TRUE
      while(unfinished){
        temp<-vector[(x-extendedLength+1):x]
        temp<-cleanVectorOfImmobileValues(temp)
        if(length(temp)==meanLength){
          unfinished<-FALSE
        }else{
        if(x-extendedLength>0){
          extendedLength<-extendedLength+1
        }else{
            result[x]<-NA
            notFound<-FALSE
            break
          }
        }
      }
      if(notFound){
      result[x]<-mean(temp)
      }
      }
  }
  return(result)
}

calculateExponentialMeanOfTemporalSeries<-function(vector,meanLength){
  result<-rep(NA, length(vector))
  alpha = 2 / (meanLength + 1)
  SMA<-calculateMeanOfTemporalSeries(vector,meanLength)
  for (x in meanLength:length(vector)) {
    if(is.na(vector[x-1])|| is.na(vector[x])){
      result[x]<-NA
    }else{
    if(vector[x-1]==vector[x]){
      result[x]<-result[x-1]
    }else{
    if(is.na(SMA[x-1])){
      result[x]<-NA
    }else{
      if(is.na(result[x-1])){
      result[x]<-vector[x] * alpha  + SMA[x-1]*(1-alpha)
      }else{
      result[x]<-vector[x] * alpha  + result[x-1]*(1-alpha)
      }
    }
    }}
  }
  return(result)
}

calculateWMAOfTemporalSeries<-function(vector,meanLength){
  result<-rep(NA, length(vector))
  for (x in meanLength:length(vector)){
    if(is.na(vector[x])||is.na(vector[x-1])){
      result[x]<-NA
    }else{
      extendedLength<-meanLength
      unfinished<-TRUE
      notFound<-TRUE
      while(unfinished){
        temp<-vector[x:(x-extendedLength+1)]
        temp<-cleanVectorOfImmobileValues(temp)
        if(length(temp)==meanLength){
          unfinished<-FALSE
        }else{
        if(x-extendedLength>0){
          extendedLength<-extendedLength+1
        }else{
          result[x]<-NA
          notFound<-FALSE
          break
        }
        }
      }
      sum<-0
      for(i in 1:length(temp)){
        sum<-sum+(temp[i]*(meanLength-i+1))
      }
      total<-meanLength*(meanLength+1)/2
      if(notFound){
      result[x]<-sum/total
      }
    }
    }
  return(result)
}

calculateStandardDeviationOfTemporalSeries<-function(vector,meanLength){
  result<-rep(NA, length(vector))
  for(x in meanLength:length(vector)) {
    if(is.na(vector[x])||is.na(vector[x-1])){
      result[x]<-NA
    }else{
      extendedLength<-meanLength
      unfinished<-TRUE
      notFound<-TRUE
      while(unfinished){
        temp<-vector[(x-extendedLength+1):x]
        temp<-cleanVectorOfImmobileValues(temp)
        if(length(temp)==meanLength){
          unfinished<-FALSE
        }else{
        if(x-extendedLength>0){
          extendedLength<-extendedLength+1
        }else{
            result[x]<-NA
            notFound<-FALSE
            break
          }
        }
      if(notFound){
      result[x]<-sd(temp)
      }
    }
  }}
  return(result)
}

calculateZScoreOfTemporalSeries<-function(vector,meanLength){
  result<-rep(NA, length(vector))
  wma<-calculateWeightedMeanOfTemporalSeries(vector,meanLength)
  standardDev<-calculateStandardDeviationOfTemporalSeries(vector,meanLength)
  for(x in meanLength:length(vector)) {
    if(is.na(standardDev[x]) || is.na(wma[x])){
      result[x]<-NA
    }else{
    if(standardDev[x]==0.0){
      result[x]<-0.0
    }
    else{
    result[x]<-(vector[x]-wma[x])/standardDev[x]
    }
    }
  }
  return(result)
}