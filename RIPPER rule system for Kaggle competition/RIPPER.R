#Cargo librerías necesarias para el problema
library(caret)
library(RWeka)
library(readr)
library(mice)

#Cargo el dataset de entrenamiento, con la fecha como date, las variables categóricas serán convertidas a factor después del preprocesamiento y limpieza.
train_values <- read_csv("train values.csv", col_types = cols(date_recorded = col_date(format = "%Y-%m-%d")))
train_labels <- read_csv("train labels.csv", col_types = cols(status_group = col_factor(levels = c("functional needs repair","functional","non functional"))))

#Combino las clases con el conjunto de datos y quito los ID del dataset para más fácil manejo
dataSet <- cbind(train_values[2:40],train_labels[,2])
set.seed(4) #Para eliminar aleatoriedad en múltiples ejecuciones del código relacionado con el muestreo

#Selección de características basado en pruebas anteriores y EDA
dataSet<-dataSet[,c("date_recorded","construction_year","funder","installer","basin","region","gps_height","population","scheme_management","extraction_type_class","payment_type","quality_group","quantity_group","source_type","waterpoint_type","status_group")]

#Tamaño de la muestra a utilizar en caso de no poder trabajar con todo el dataset
#dataSet<-dataSet[sample(1:nrow(dataSet),size=20000),]

#Convierto las variables categoricas a factores para poder ser procesadas
dataSet$funder<-as.factor(dataSet$funder)
dataSet$installer<-as.factor(dataSet$installer)
dataSet$basin<-as.factor(dataSet$basin)
dataSet$region<-as.factor(dataSet$region)
dataSet$scheme_management<-as.factor(dataSet$scheme_management)
dataSet$extraction_type_class<-as.factor(dataSet$extraction_type_class)
dataSet$payment_type<-as.factor(dataSet$payment_type)
dataSet$quality_group<-as.factor(dataSet$quality_group)
dataSet$quantity_group<-as.factor(dataSet$quantity_group)
dataSet$source_type<-as.factor(dataSet$source_type)
dataSet$waterpoint_type<-as.factor(dataSet$waterpoint_type)
dataSet$status_group<-as.factor(dataSet$status_group)

#Reduzco los níveles de Funder a 15
funderNames <- names(summary(dataSet$funder)[1:14])
funder <- factor(dataSet$funder, levels=c(funderNames, "Other"))
funder[is.na(funder)] <- "Other"
dataSet$funder <- funder

#Reduzco los níveles de installer a 12
installerNames <- names(summary(dataSet$installer)[1:11])
installer <- factor(dataSet$installer, levels=c(installerNames, "Other"))
installer[is.na(installer)] <- "Other"
dataSet$installer <- installer

#Convierto las fechas de creación de los pozos en númerico basados en el año para posterior procesamiento
dataSet$date_recorded<-as.numeric(substr(dataSet$date_recorded,1,4))

#Ya que solo existen 31 pozos anteriores al 2011, con tal de recortar el número de níveles a procesar por las reglas y mejorar la precisión, convierto los anteriores a 2011
dataSet$date_recorded[dataSet$date_recorded<2011]<-2011
dataSet$date_recorded<-as.factor(dataSet$date_recorded) #convierto a factor los años

#Tratamiento de valores NA mediante imputación con la librería mice
imputados <- mice::mice(dataSet, m=5, meth="pmm" )
dataSet <- mice::complete(imputados)

#Discretizo las variables continuas con los puntos de corte determinados por el método Ameva en estudios anteriores
dataSet$gps_height<-cut(dataSet$gps_height,breaks=c(-Inf,1359.5,1932.5,Inf),labels=c("Underground","Surface","Very high located"))
dataSet$population<-cut(dataSet$population,breaks=c(-Inf,0.5,1.5,Inf),labels=c("Unpopulated","Singe Person","Village"))
dataSet$construction_year<-cut(dataSet$construction_year,breaks=c(-Inf,980,1998.5,Inf),labels=c("Ancient","Old","Modern"))

# Aplico el algoritmo Ripper
model.Ripper = JRip(status_group~., dataSet)
summary(model.Ripper) #summary del modelo Ripper en JWeka funciona usando cross-validation para la precisión

# Cargo los valores de prueba
test_values <- read_csv("test values.csv", col_types = cols(date_recorded = col_date(format = "%Y-%m-%d")))
test_values<-test_values[,c("id","date_recorded","construction_year","funder","installer","basin","region","gps_height","population","scheme_management","extraction_type_class","payment_type","quality_group","quantity_group","source_type","waterpoint_type")]

#Convierto las variables categoricas a factores para poder ser procesados
test_values$funder<-as.factor(test_values$funder)
test_values$installer<-as.factor(test_values$installer)
test_values$basin<-as.factor(test_values$basin)
test_values$region<-as.factor(test_values$region)
test_values$scheme_management<-as.factor(test_values$scheme_management)
test_values$extraction_type_class<-as.factor(test_values$extraction_type_class)
test_values$payment_type<-as.factor(test_values$payment_type)
test_values$quality_group<-as.factor(test_values$quality_group)
test_values$quantity_group<-as.factor(test_values$quantity_group)
test_values$source_type<-as.factor(test_values$source_type)
test_values$waterpoint_type<-as.factor(test_values$waterpoint_type)

#Reduzco los níveles de Funder a 15 usando los nombres utilizados durante el entrenamiento
funder <- factor(test_values$funder, levels=c(funderNames, "Other"))
funder[is.na(funder)] <- "Other"
test_values$funder <- funder

#Reduzco los níveles de Installer a 12 usando los nombres utilizados durante el entrenamiento
installer <- factor(test_values$installer, levels=c(installerNames, "Other"))
installer[is.na(installer)] <- "Other"
test_values$installer <- installer

#Tratamiento de valores NA mediante imputación con la librería mice
imputados <- mice::mice(test_values, m=5, meth="pmm")
test_values <- mice::complete(imputados)

#Discretizo las variables continuas con los mismos puntos de corte del conjunto de entrenamiento
test_values$gps_height<-cut(test_values$gps_height,breaks=c(-Inf,1359.5,1932.5,Inf),labels=c("Underground","Surface","Very high located"))
test_values$population<-cut(test_values$population,breaks=c(-Inf,0.5,1.5,100,Inf),labels=c("Unpopulated","Singe Person","Village"))
test_values$construction_year<-cut(test_values$construction_year,breaks=c(-Inf,980,1998.5,Inf),labels=c("Ancient","Old","Modern"))

#Uso del modelo para predecir los nuevos valores.
model.Ripper.pred = predict(model.Ripper, newdata = test_values)

#Preparación y escritura de los datos
df<-data.frame(test_values$id,model.Ripper.pred)
colnames(df)<-c("id","status_group")
write_csv(df,path="test_values.csv")
