# cargar datos
datos <- read.csv("datos/train.csv", 
                  header = TRUE, sep=",", 
                  na.strings=c(".","NA","","?", "unknown", "Unknown", "-"))

test <-  read.csv("datos/test.csv", sep =",", header = TRUE)

table(datos$y)

length(datos$y)

clases=as.integer(sort(unique(datos$y)))
clases

numClases = length(clases)

limite = numClases-1

numCol = ncol(datos)

li = list()
numObs= (numClases*limite)/2
df.table = as.data.frame(matrix(numeric(),nrow = length(numObs)))
index_li= 1

for (i in 1:limite) {
  indices_i = which(datos[, numCol]==clases[i])
  
  j = i + 1
  for (z in j:numClases) {
    # print(paste("Matriz: ", as.character(clases[i]), "contra", as.character(clases[z])));
    
    indices_z = which(datos[, numCol]==clases[z])
    
    df = data.frame(rbind(datos[indices_i, ], datos[indices_z, ]))
    #print(df$y)
    #print(table(df$y))
    li[[index_li]] = df
    t = table(df$y)
    #df.table = rbind(df.table, cbind(t[1], t[2]))
    #print(row.names(df.table))#[index_li] = cat("hola", as.character(index_li))
    index_li = index_li + 1
  }
  
}


numObs = 5
df.probs.test = as.data.frame(matrix(numeric(),nrow = length(numObs)))
for(i in 1:numObs) {
  df = li[[i]]
  
  mod.rf = randomForest::randomForest(x = df[, -numCol],
                                      y = as.factor(df[, "y"]),
                                      ntree = 500, keep.forest = TRUE)
  
  probs = predict(mod.rf, test[,-numCol], type = "prob")
  
  df.probs.test = cbind(df.probs.test, probs[,1])
  
  
  
  
}
dim(test)
dim(df.probs.test)
print(df.probs.test)

#rm(df.table)
rm(df.probs.test)
