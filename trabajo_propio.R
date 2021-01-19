#Cargamos las librerias necesarias para el problema
library(caret)
library(tree)
library(RWeka)
library(readr)
library(mice)

#Cargamos el dataset, con valores de fecha como date
train_values=read_csv("training.set.values.csv", col_types = cols(date_recorded = col_date(format = "%Y-%m-%d")))
train_labels <- read_csv("training.set.labels.csv", col_types = cols(status_group = col_factor(levels = c("functional needs repair","functional","non functional"))))


#Fijo la semilla y combino los archivos anteriores
dataSet <- cbind(train_values[2:40],train_labels[,2])
set.seed(4)


#hacemos una primera limpieza de nuestros datos. Con ello nos quedaremos con los que consideremos de importancia medio o alta.
datos= dataSet[,c("date_recorded" , "construction_year" , "funder" , "installer" , "basin" , "region", "gps_height" , "population" , "scheme_management" , "extraction_type_class", "management_group" , "payment_type" , "quality_group" , "quantity" , "source_type" , "waterpoint_type" , "status_group")]


############PREPROCESAMIENTO#####################

#Empiezo por quedarme solo con los casos completos
#datos= datos[complete.cases(datos),]

#Convierto las variables categóricas a factores para su posterior tratamiento
datos$funder=as.factor(datos$funder)
datos$installer= as.factor(datos$installer)
datos$basin=as.factor(datos$basin)
datos$region=as.factor(datos$region)
datos$lga = as.factor(datos$lga)
datos$scheme_management= as.factor(datos$scheme_management)
datos$extraction_type=as.factor( datos$extraction_type)
datos$extraction_type_class = as.factor(datos$extraction_type_class)
datos$management= as.factor(datos$management)
datos$management_group = as.factor(datos$management_group)
datos$payment_type= as.factor(datos$payment_type)
datos$quality_group= as.factor(datos$quality_group)
datos$quantity= as.factor(datos$quantity)
datos$source_type= as.factor(datos$source_type)
datos$waterpoint_type= as.factor(datos$waterpoint_type)
datos$status_group= as.factor(datos$status_group)


#Eliminacion de variables segun nuestros intereses
datos$construction_year.1=NULL #Variable repetida


#Reducimos los niveles de funder a 20
fundernames= names(summary(datos$funder)[1:14])
funder <- factor(datos$funder, levels=c(fundernames, "Other"))
funder[is.na(funder)] <- "Other"
datos$funder <- funder

#Repetimos el proceso con installer
installerNames <- names(summary(datos$installer)[1:15])
installer <- factor(datos$installer, levels=c(installerNames, "Other"))
installer[is.na(installer)] <- "Other"
datos$installer <- installer

#Repetimos el proceso con lga
#lganames= names(summary(datos$lga)[1:20])
#lga= factor(datos$lga , levels=c(lganames , "other"))
#lga[is.na(lga)] <- "Other"
#datos$lga <- lga

#Convertimos las fechas de creacion del pozo en variable numerica quedandome solo con  el año
datos$date_recorded = as.numeric (substr(datos$date_recorded,1,4))
                                 
#DATO IMPORTANTE: Como solo existen 31 fechas por debajo del 2011, todas esas se convertirán a 2011
datos$date_recorded[datos$date_recorded < 2011] = 2011
datos$date_recorded = as.factor(datos$date_recorded) #Conversion a factor de los años

#Tratamiento de valores NA mediante imputaciÃ³n con la librerÃ?a mice
imputados <- mice::mice(datos, m=5, meth="pmm" )
datos <- mice::complete(imputados)

#Discretizamos las variables numéricas aplicando: "Recursive discretization using gain ratio for multi-class variable"
library(funModeling)
library(dplyr)
datos.para.discretizar=datos #Copiamos el dataset para evitar filtraciones de variables
datos.para.discretizar$gps_height_2= discretize_rgr(input=datos.para.discretizar$gps_height , target=datos.para.discretizar$gps_height)
summary(datos.para.discretizar$gps_height_2) #Vemos donde localizar los puntos
datos.para.discretizar$population_2=discretize_rgr(input=datos.para.discretizar$population , target=datos.para.discretizar$population)
summary(datos.para.discretizar$population_2)
datos.para.discretizar$construction_year_2=discretize_rgr(input= datos.para.discretizar$construction_year , target= datos.para.discretizar$construction_year)
summary(datos.para.discretizar$construction_year_2)

#Convertimos las variables numericas a categoricas con los cortes encontrados anteriormente
datos$gps_height= cut(datos$gps_height, breaks=c(-Inf,389,1430,Inf), labels=c("Bajo" , "Medio" , "Alto"))
datos$population= cut(datos$population, breaks=c(-Inf,1,450,Inf) , labels=c("Poblacion nula" , "Poblacion media" , "Poblacion alta"))
datos$construction_year= cut(datos$construction_year, breaks=c(-Inf,1999,Inf) , labels=c("Antes del 1999" , "Despues de 1999"))


#Aplico el modelo C4.5
model.C4.5= J48(status_group~. , datos)
summary(model.C4.5)

#Cargo los valores de testing
test_values <- read_csv("test.set.values.csv", col_types = cols(date_recorded = col_date(format = "%Y-%m-%d")))
datos_prueba= test_values[,c("id" , "date_recorded" , "construction_year" , "funder" , "installer" , "basin" , "region", "gps_height" , "population" , "scheme_management", "extraction_type" , "extraction_type_class", "management" , "management_group" , "payment_type" , "quality_group" , "quantity" , "source_type" , "waterpoint_type")]
datos_prueba$status_group= datos$status_group[1:length(datos_prueba$construction_year)]

#Empiezo por quedarme solo con los casos completos
datos_prueba= datos_prueba[complete.cases(datos_prueba),]



#Convierto variables categoricas a factores
datos_prueba$funder=as.factor(datos_prueba$funder)
datos_prueba$installer= as.factor(datos_prueba$installer)
datos_prueba$basin=as.factor(datos_prueba$basin)
datos_prueba$region=as.factor(datos_prueba$region)
datos_prueba$lga = as.factor(datos_prueba$lga)
datos_prueba$scheme_management= as.factor(datos_prueba$scheme_management)
datos_prueba$extraction_type=as.factor( datos_prueba$extraction_type)
datos_prueba$extraction_type_class = as.factor(datos_prueba$extraction_type_class)
datos_prueba$management= as.factor(datos_prueba$management)
datos_prueba$management_group = as.factor(datos_prueba$management_group)
datos_prueba$payment_type= as.factor(datos_prueba$payment_type)
datos_prueba$quality_group= as.factor(datos_prueba$quality_group)
datos_prueba$quantity= as.factor(datos_prueba$quantity)
datos_prueba$source_type= as.factor(datos_prueba$source_type)
datos_prueba$waterpoint_type= as.factor(datos_prueba$waterpoint_type)




#Reducimos los niveles de funder a 20
fundernames_prueba= names(summary(datos_prueba$funder)[1:14])
funder <- factor(datos_prueba$funder, levels=c(fundernames_prueba, "Other"))
funder[is.na(funder)] <- "Other"
datos_prueba$funder <- funder

#Repetimos el proceso con installer
installerNames <- names(summary(datos_prueba$installer)[1:15])
installer <- factor(datos_prueba$installer, levels=c(installerNames, "Other"))
installer[is.na(installer)] <- "Other"
datos_prueba$installer <- installer

#Repetimos el proceso con lga
#lganames_prueba= names(summary(datos_prueba$lga)[1:20])
#lga= factor(datos_prueba$lga , levels=c(lganames_prueba , "other"))


#Convertimos las fechas de creacion del pozo en variable numerica quedandome solo con  el año
datos_prueba$date_recorded = as.numeric (substr(datos_prueba$date_recorded,1,4))

#DATO IMPORTANTE: Como solo existen 31 fechas por debajo del 2011, todas esas se convertirán a 2011
datos_prueba$date_recorded[datos_prueba$date_recorded < 2011] = 2011
datos_prueba$date_recorded = as.factor(datos_prueba$date_recorded) #Conversion a factor de los años


#Tratamiento de valores NA mediante imputaciÃ³n con la librerÃ?a mice
#imputados <- mice::mice(datos_prueba, m=5, meth="pmm")
#datos_prueba <- mice::complete(imputados)

#Convertimos las variables numericas a categoricas con los cortes encontrados anteriormente
datos_prueba$gps_height= cut(datos_prueba$gps_height, breaks=c(-Inf,389,1430,Inf), labels=c("Bajo" , "Medio" , "Alto"))
datos_prueba$population= cut(datos_prueba$population, breaks=c(-Inf,1,450,Inf) , labels=c("Poblacion nula" , "Poblacion media" , "Poblacion alta"))
datos_prueba$construction_year= cut(datos_prueba$construction_year, breaks=c(-Inf,1999,Inf) , labels=c("Antes del 1999" , "Despues de 1999"))

#Uso del modelo para predecir los nuevos valores.
model.C4.5.pred= predict(model.C4.5, newdata= datos_prueba)



#Visualizacion de la matriz de confusion
confusionMatrix(model.C4.5.pred, datos$status_group[1:length(model.C4.5.pred)])


#implementacion de cv
set.seed(4)
datos_prueba$status_group=datos$status_group[1:length(datos_prueba$date_recorded)] #Meto mis la variable clase en el test para que RWeka haga las predicciones
cv_model=evaluate_Weka_classifier(model.C4.5 , newdata=datos_prueba , numFolds = 10, complexity = TRUE , class = TRUE)
cv_model$details[1]



#Limpieza de datos2:
set.seed(4)
datos2= datos[,c("date_recorded" , "construction_year", "installer" , "basin" , "region", "gps_height" , "population" , "scheme_management" , "extraction_type_class", "management_group" , "payment_type" , "quality_group" , "quantity" , "source_type" , "waterpoint_type" , "status_group")]
model.C4.5_2= J48(status_group~. , datos2)
datos2_prueba= datos_prueba[,c("id","date_recorded" , "construction_year", "installer" , "basin" , "region", "gps_height" , "population" , "scheme_management" , "extraction_type_class", "management_group" , "payment_type" , "quality_group" , "quantity" , "source_type" , "waterpoint_type")]
datos2_prueba$status_group=datos$status_group[1:length(datos2_prueba$date_recorded)]
model.C4.5_2.pred= predict(model.C4.5_2, newdata=datos2_prueba)
confusionMatrix(model.C4.5_2.pred, datos$status_group[1:length(model.C4.5_2.pred)])
cv_model2=evaluate_Weka_classifier(model.C4.5_2, newdata=datos2_prueba , numFolds = 10 , complexity = TRUE , class=TRUE)
cv_model2$details[1]

#limpieza datos3:
set.seed(4)
datos3= datos[,c("date_recorded" , "construction_year", "installer" , "basin", "region",  "gps_height" , "population" , "scheme_management" , "extraction_type_class" , "payment_type" , "quantity", "quality_group", "source_type" ,"waterpoint_type", "status_group")]
datos3_prueba= datos_prueba[,c("id","date_recorded" , "construction_year", "installer" , "basin", "region",  "gps_height" , "population" , "scheme_management" , "extraction_type_class" , "payment_type" , "quantity", "quality_group", "source_type" ,"waterpoint_type")]
model.C4.5_3=J48(status_group~., data=datos3)
datos3_prueba$status_group=datos3$status_group[1:length(datos3_prueba$date_recorded)]
cv_model3=evaluate_Weka_classifier(model.C4.5_3, newdata=datos3_prueba , numFolds = 10 , complexity = TRUE , class=TRUE)
cv_model3$details[1]


#limpieza se datos4:
set.seed(4)
datos4= datos[,c("date_recorded" , "construction_year",  "gps_height" , "population", "extraction_type_class" , "payment_type" , "quantity", "source_type" ,"waterpoint_type", "status_group")]
datos4_prueba= datos_prueba[,c("date_recorded" , "construction_year",  "gps_height" , "population", "extraction_type_class" , "payment_type" , "quantity", "source_type" ,"waterpoint_type")]
model.C4.5_4=J48(status_group~., data=datos4)
datos4_prueba$status_group=datos4$status_group[1:length(datos4_prueba$date_recorded)]
cv_model4=evaluate_Weka_classifier(model.C4.5_4, newdata=datos4_prueba , numFolds = 10 , complexity = TRUE , class=TRUE)
cv_model4$details[1]

#limpieza datos5:
set.seed(4)
datos5= datos[,c("date_recorded" , "construction_year",  "gps_height" , "population", "extraction_type_class", "quantity", "source_type", "status_group")]
datos5_prueba= datos_prueba[,c("date_recorded" , "construction_year",  "gps_height" , "population", "extraction_type_class", "quantity", "source_type")]
model.C4.5_5=J48(status_group~., data=datos5)
datos5_prueba$status_group=datos5$status_group[1:length(datos5_prueba$date_recorded)]
cv_model5=evaluate_Weka_classifier(model.C4.5_5, newdata=datos5_prueba , numFolds = 10 , complexity = TRUE , class=TRUE)
cv_model5$details[1]

#limpieza datos6:
set.seed(4)
datos6= datos[,c("date_recorded" , "construction_year",  "gps_height" , "population", "quantity", "status_group")]
datos6_prueba= datos_prueba[,c("date_recorded" , "construction_year",  "gps_height" , "population", "quantity")]
model.C4.5_6=J48(status_group~., data=datos6)
datos6_prueba$status_group=datos6$status_group[1:length(datos6_prueba$date_recorded)]
cv_model6=evaluate_Weka_classifier(model.C4.5_6, newdata=datos6_prueba , numFolds = 10 , complexity = TRUE , class=TRUE)
cv_model6$details[1]


#Pruebas con subsets

#1#
set.seed(4)
subset3<-datos3[sample(1:nrow(datos3),size=5000),]
subset3_prueba<-datos3_prueba[sample(1:nrow(datos3),size=1000),]
sub_model3=J48(status_group~., data=subset3)
sub_cv_model3=evaluate_Weka_classifier(sub_model3, newdata=subset3_prueba , numFolds = 10 , complexity = TRUE , class=TRUE)
sub_cv_model3$details[1] 


#2#
set.seed(4)
subset2<-datos2[sample(1:nrow(datos3),size=20000),]
subset2_prueba<-datos2_prueba[sample(1:nrow(datos2),size=9000),]
sub_model2=J48(status_group~., data=subset2)
sub_cv_model2=evaluate_Weka_classifier(sub_model2, newdata=subset2_prueba , numFolds = 10 , complexity = TRUE , class=TRUE)
sub_cv_model2$details[1]





#Preparacion y escritura de los datos
df=data.frame(datos_prueba$id, model.C4.5.pred)
colnames(df)<-c("id","status_group")
write.csv(df,file="test.set.values.csv")

