ProcesarDatosOVO<-function(dataframe,pos){ #45 datasets
  #Convertir a 0,1 para ciertos algoritmos
  train<-dataframe
  train$y<-as.factor(train$y)
  x<-sum(train$y==levels(train$y)[1])
  y<-sum(train$y==levels(train$y)[2])
  if(x>y){
    max<-levels(train$y)[1]
    min<-levels(train$y)[2]
    train$y<-ifelse(train$y==(as.integer(max)),1,0)
  }else{
    max<-levels(train$y)[2]
    min<-levels(train$y)[1]
    train$y<-ifelse(train$y==(as.integer(min)),0,1)
  }
  train$y<-as.factor(train$y)

  #USAR ALGORITMOS AQUÍ

  
  
  #Reconvertir de vuelta a las clases originales
  train$y<-as.factor(ifelse(train$y==1,max,min))

  if(class(dataframe$y)=="numeric"){
    train$y<-as.integer(train$y)-1
  }
  return(train) 
}


ProcesarDatosOVA<-function(dataframe,pos){ #10 datasets
  train<-dataframe
  train$y<-as.factor(train$y)
  #USAR ALGORITMOS AQUÍ
  set.seed(seed)
  #synth<-IPF(train, nfolds = 5, p = 0.01, s = 3,y = 0.5)
  #train<-synth$cleanData
  
  if(class(dataframe$y)=="numeric"){
    train$y<-as.integer(train$y)-1
  }
  return(train)
}
